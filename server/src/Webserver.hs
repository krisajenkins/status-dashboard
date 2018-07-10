{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Webserver
  ( app
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (Value(Number), encode, object)
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack)
import Development.GitRev (gitHash)
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
import Servant.Server (Handler, Server)
import Webserver.API (Web)
import Webserver.Types (OverallStatus(Bad, Good))

api :: Handler Text
api = pure "Hello"

websocket :: Server WebSocket
websocket = serveWebsocket
  where
    serveWebsocket :: MonadIO m => Connection -> m ()
    serveWebsocket connection =
      liftIO $ do
        forkPingThread connection 10
        for_ [1 .. 10] $ \i -> do
          sendTextData connection $
            encode $
            if i < 8
              then Good
              else Bad
          threadDelay 1000000

version :: Applicative m => m Text
version = pure $(gitHash)

server :: FilePath -> Server Web
server staticDir =
  version :<|> api :<|> websocket :<|> serveDirectoryFileServer staticDir

app :: FilePath -> Application
app staticDir = middleware $ serve (Proxy :: Proxy Web) (server staticDir)

middleware :: Middleware
middleware = gzip def . logStdout . simpleCors
