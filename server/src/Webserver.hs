{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Webserver
  ( app
  ) where

import Webserver.API (Web)

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (encode)
import Data.Default.Class (def)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Development.GitRev (gitHash)
import Network.Wai (Application, Middleware)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
  ( (:<|>)((:<|>))
  , ServantErr
  , err500
  , errBody
  , serve
  , serveDirectoryFileServer
  )
import Servant.Server (Handler, Server)
import Webserver.API (Web)

api :: Handler Text
api = pure "Hello"

version :: Applicative m => m Text
version = pure $(gitHash)

server :: FilePath -> Server Web
server staticDir = version :<|> api :<|> serveDirectoryFileServer staticDir

app :: FilePath -> Application
app staticDir = middleware $ serve (Proxy :: Proxy Web) (server staticDir)

middleware :: Middleware
middleware = gzip def . logStdout . simpleCors
