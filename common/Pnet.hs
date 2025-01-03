module Pnet
  ( Handshake (..),
    Self (..),
    Response (..),
    pnetSocketAddr,
    pnetSocket,
    withPnetSocket,
    timeout,
    bufferSize,
    queueSize,
    Transport (..),
  )
where

import Control.Applicative ((<|>))
import Control.Exception
import Data.Maybe
import Data.Serialize
import Debug.Trace
import GHC.Generics
import Network.Socket
import Pnet.Routing
import System.Environment
import System.Posix

data Transport
  = Stdio
  | Process String
  deriving stock (Eq, Show, Generic)

newtype Self = Self {unSelf :: Address}
  deriving stock (Show, Generic)

data Handshake where
  ConnectNode :: Transport -> Maybe Address -> Handshake
  ListNodes :: Handshake
  Route :: Handshake
  TunnelProcess :: Handshake
  deriving stock (Show, Generic)

data Response where
  NodeList :: [Address] -> Response
  deriving stock (Show, Generic)

timeout :: Int
timeout = 16384

bufferSize :: Int
bufferSize = 8192

queueSize :: Int
queueSize = 16

defaultPnetSocketPath :: FilePath
defaultPnetSocketPath = "/run/pnet.sock"

defaultUserPnetSocketPath :: IO FilePath
defaultUserPnetSocketPath = go <$> getEffectiveUserID
  where
    go 0 = defaultPnetSocketPath
    go n = concat ["/run/user/", show n, "/pnet.sock"]

pnetSocket :: IO Socket
pnetSocket = do
  s <- socket AF_UNIX Stream defaultProtocol
  setSocketOption s RecvTimeOut timeout
  setSocketOption s SendTimeOut timeout
  pure s

pnetSocketAddr :: Maybe FilePath -> IO SockAddr
pnetSocketAddr customPath = do
  defaultPath <- defaultUserPnetSocketPath
  extraCustomPath <- lookupEnv "PNET_SOCKET_PATH"
  let path = fromMaybe defaultPath (customPath <|> extraCustomPath)
  traceM ("comunicating over \"" <> path <> "\"")
  pure $ SockAddrUnix path

withPnetSocket :: (Socket -> IO a) -> IO a
withPnetSocket = bracket pnetSocket close

instance Serialize Transport

instance Serialize Self

instance Serialize Handshake

instance Serialize Response
