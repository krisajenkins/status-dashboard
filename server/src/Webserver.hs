{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Webserver
  ( app
  ) where

import Control.Concurrent (threadDelay)
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
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.WebSockets (Connection, forkPingThread, sendTextData)
import Servant
  ( (:<|>)((:<|>))
  , ServantErr
  , err500
  , errBody
  , serve
  , serveDirectoryFileServer
  )
import Servant.API.WebSocket (WebSocket)
import Servant.Client (BaseUrl(BaseUrl), Scheme(Https), ServantError)
import Servant.Server (Handler, Server)
import Webserver.API (Web)
import Webserver.Types (OverallStatus(Bad, Good, Unknown))

api :: Handler Text
api = pure "Hello"

summarise :: Either ServantError HealthcheckResponse -> OverallStatus
summarise (Left _) = Unknown
summarise (Right response) =
  if has (checks . traversed . _Error) response
    then Bad
    else Good

healthcheckUrl = BaseUrl Https "staging.iele.dev-mantis.iohkdev.io" 8546 ""

websocket :: Server WebSocket
websocket = serveWebsocket
  where
    serveWebsocket :: MonadIO m => Connection -> m ()
    serveWebsocket connection =
      liftIO $ do
        manager <- newManager tlsManagerSettings
        forkPingThread connection 10
        forever $ do
          response <- Healthcheck.run manager healthcheckUrl getHealthcheck
          sendTextData connection $ encode $ summarise response
          threadDelay 5000000

version :: Applicative m => m Text
version = pure $(gitHash)

server :: FilePath -> Server Web
server staticDir =
  version :<|> api :<|> websocket :<|> serveDirectoryFileServer staticDir

app :: FilePath -> Application
app staticDir = middleware $ serve (Proxy :: Proxy Web) (server staticDir)

middleware :: Middleware
middleware = gzip def . logStdout . simpleCors
