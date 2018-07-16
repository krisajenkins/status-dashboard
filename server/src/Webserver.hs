{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Webserver
  ( run
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.BroadcastChan
  ( BroadcastChan
  , In
  , newBChanListener
  , newBroadcastChan
  , readBChan
  , writeBChan
  )
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.), contains, has, traversed)
import Control.Monad (forever)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (logDebug, logDebugN, runStderrLoggingT)
import Data.Aeson (Value(Number), encode, object)
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Development.GitRev (gitHash)
import Healthcheck (HealthcheckResponse, _Error, checks, getHealthcheck)
import qualified Healthcheck
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (Settings, runSettings)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.WebSockets (Connection, forkPingThread, sendTextData)
import Servant ((:<|>)((:<|>)), serve, serveDirectoryFileServer)
import Servant.API.WebSocket (WebSocket)
import Servant.Client (BaseUrl(BaseUrl), Scheme(Https), ServantError)
import Servant.Server (Handler, Server)
import Webserver.API (Web)
import Webserver.Types (OverallStatus(Bad, Good, Unknown))

api :: Handler Text
api = pure "Hello"

type Response = Either ServantError HealthcheckResponse

summarise :: Response -> OverallStatus
summarise (Left _) = Unknown
summarise (Right response) =
  if has (checks . traversed . _Error) response
    then Bad
    else Good

healthcheckUrl :: BaseUrl
healthcheckUrl = BaseUrl Https "staging.iele.dev-mantis.iohkdev.io" 8546 ""

websocket :: BroadcastChan In Response -> Server WebSocket
websocket broadcaster = serveWebsocket
  where
    serveWebsocket :: MonadIO m => Connection -> m ()
    serveWebsocket connection = do
      liftIO $ do
        chan <- newBChanListener broadcaster
        forkPingThread connection 10
        forever $ do
          response <- readBChan chan
          sendTextData connection $ encode $ summarise response

version :: Applicative m => m Text
version = pure $(gitHash)

server :: BroadcastChan In Response -> FilePath -> Server Web
server broadcaster staticDir =
  version :<|> api :<|> websocket broadcaster :<|>
  serveDirectoryFileServer staticDir

watcher :: BroadcastChan In Response -> IO ()
watcher broadcaster = do
  manager <- newManager tlsManagerSettings
  forever $ do
    response <- Healthcheck.run manager healthcheckUrl getHealthcheck
    writeBChan broadcaster response
    threadDelay 5000000

app :: BroadcastChan In Response -> FilePath -> Application
app broadcaster staticDir = do
  middleware $ serve (Proxy :: Proxy Web) (server broadcaster staticDir)

run :: MonadIO m => Settings -> FilePath -> m ()
run settings staticDir = liftIO $ do
  broadcaster <- newBroadcastChan
  forkIO $ Webserver.watcher broadcaster
  runSettings settings $ Webserver.app broadcaster staticDir

middleware :: Middleware
middleware = gzip def . logStdout . simpleCors
