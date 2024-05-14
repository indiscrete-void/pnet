import Data.Functor
import Network.Socket (bind, listen)
import Pnet
import Pnet.Options
import Polysemy hiding (run, send)
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.Extra.Trace
import Polysemy.Fail
import Polysemy.Serialize
import Polysemy.Socket
import Polysemy.Trace
import Polysemy.Transport
import System.Exit
import System.Posix
import Text.Printf qualified as Text

type State = [NodeID]

initialState :: State
initialState = []

pnetd :: (Member Trace r, Member (Socket ManagerToNodeMessage NodeToManagerMessage s) r, Member Async r, Member (AtomicState State) r) => Sem r ()
pnetd = handleClient @ManagerToNodeMessage @NodeToManagerMessage $ handle go >> close
  where
    go ListNodes = do
      nodeList <- atomicGet @State
      traceTagged "ListNodes" (Text.printf "responding with `%s`" (show nodeList))
      output (NodeList nodeList)
    go (NodeAvailability transport maybeNode) = case maybeNode of
      Just nodeID -> atomicModify' (nodeID :) >> traceTagged "NodeAvailability" (Text.printf "%s connected over `%s`" (show nodeID) (show transport))
      Nothing -> traceTagged "NodeAvailability" (Text.printf "unknown node connected over `%s`" (show transport))

forkIf :: Bool -> IO () -> IO ()
forkIf True m = forkProcess m >> exitSuccess
forkIf False m = m

main :: IO ()
main =
  let runSocket server =
        sockToIO bufferSize server
          . runDecoder
          . unserializeSock @ManagerToNodeMessage @NodeToManagerMessage
      runAtomicState = void . atomicStateToIO initialState
      run server = runFinal @IO . asyncToIOFinal . embedToFinal @IO . failToEmbed @IO . traceToStdoutBuffered . runSocket server . runAtomicState
   in withPnetSocket \sock -> do
        (Options maybeSocketPath daemon) <- parse
        addr <- pnetSocketAddr maybeSocketPath
        bind sock addr
        listen sock 5
        forkIf daemon $ run sock pnetd
