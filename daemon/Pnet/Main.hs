import Control.Monad
import Data.List qualified as List
import Network.Socket (bind, listen)
import Pnet
import Pnet.Node
import Pnet.Options
import Pnet.Routing
import Polysemy hiding (run, send)
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.Extra.Trace
import Polysemy.Fail
import Polysemy.ScopedBundle
import Polysemy.Serialize
import Polysemy.Socket
import Polysemy.Socket.Accept
import Polysemy.Sockets
import Polysemy.Trace
import Polysemy.Transport
import System.Exit
import System.Posix
import Text.Printf qualified as Text

type State s = [(s, Address)]

initialState :: State s
initialState = []

whenJust :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
whenJust = maybe (pure ())

pnetd :: (Member (Accept s) r, Member (Sockets Request Response s) r, Member (Sockets RoutedFrom RouteTo s) r, Member (AtomicState (State s)) r, Member Trace r, Member Fail r, Member Decoder r, Member Async r, Eq s) => Sem r ()
pnetd = foreverAcceptAsync \s -> socket s (handle (go s) >> close)
  where
    go _ ListNodes = do
      nodeList <- map snd <$> atomicGet
      traceTagged "ListNodes" (Text.printf "responding with `%s`" (show nodeList))
      output (NodeList nodeList)
    go s (ConnectNode transport maybeNodeID) = do
      traceTagged "NodeAvailability" (Text.printf "%s connected over `%s`" nodeIDStr (show transport))
      whenJust (atomicModify' . (:) . entry) maybeNodeID
      traceTagged "pnetnd" . show =<< runFail (socket s $ runR2 tmpAddr pnetnd')
      traceTagged "NodeAvailability" (Text.printf "%s disconnected from `%s`" nodeIDStr (show transport))
      whenJust (atomicModify' . List.delete . entry) maybeNodeID
      where
        nodeIDStr = maybe "unknown node" show maybeNodeID
        entry nodeID = (s, nodeID)
    go _ _ = _

main :: IO ()
main =
  let runUnserialized :: (Member Fail r, Member Decoder r, Member ByteInputWithEOF r, Member ByteOutput r) => InterpretersFor (InputWithEOF Request ': Output Response ': '[]) r
      runUnserialized = serializeOutput @Response . deserializeInput @Request
      runUnserialized' :: (Member Fail r, Member Decoder r, Member ByteInputWithEOF r, Member ByteOutput r) => InterpretersFor (InputWithEOF RoutedFrom ': Output RouteTo ': '[]) r
      runUnserialized' = serializeOutput @RouteTo . deserializeInput @RoutedFrom
      runTransport f s = closeToSocket timeout s . outputToSocket s . inputToSocket bufferSize s . f . raise2Under @ByteInputWithEOF . raise2Under @ByteOutput
      runSocket s = acceptToIO s . runScopedBundle @(SocketEffects Request Response) (runTransport runUnserialized) . runScopedBundle @(SocketEffects RoutedFrom RouteTo) (runTransport runUnserialized')
      runAtomicState = void . atomicStateToIO initialState
      run s =
        runFinal @IO
          . asyncToIOFinal
          . runDecoder
          . embedToFinal @IO
          . failToEmbed @IO
          . traceToStdoutBuffered
          . runSocket s
          . runAtomicState
      forkIf True m = forkProcess m >> exitSuccess
      forkIf False m = m
   in withPnetSocket \s -> do
        (Options maybeSocketPath daemon) <- parse
        addr <- pnetSocketAddr maybeSocketPath
        bind s addr
        listen s 5
        forkIf daemon $ run s pnetd
