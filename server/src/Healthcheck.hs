{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Healthcheck where

import Control.Lens (makeLenses, makePrisms)
import Data.Aeson
  ( FromJSON
  , ToJSON
  , (.:)
  , (.:?)
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , object
  , parseJSON
  , toJSON
  , withArray
  , withObject
  )
import Data.Aeson.Types (Parser)
import Data.List ()
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Servant ((:>), Get, JSON)
import Servant ()
import Servant.Client
  ( BaseUrl
  , ClientEnv
  , ClientM
  , Scheme
  , ServantError
  , client
  , manager
  , mkClientEnv
  , runClientM
  )

newtype HealthcheckResponse = HealthcheckResponse
  { _checks :: [Check]
  } deriving (Show, Eq, Generic)

instance FromJSON HealthcheckResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data Check
  = Ok Text
  | Error ErrorMessage
          Text
  deriving (Show, Eq, Generic)

instance FromJSON Check where
  parseJSON =
    withObject "check" $ \obj -> do
      status <- obj .: "status"
      description <- obj .: "description"
      case status of
        "OK" -> pure $ Ok description
        "ERROR" -> do
          error <- obj .: "error"
          pure $ Error error description
        _ -> fail $ "Unknown status: " <> status

newtype ErrorMessage =
  ErrorMessage Text
  deriving (Show, Eq, Generic, FromJSON)

makeLenses ''HealthcheckResponse

makePrisms ''Check

------------------------------------------------------------
type MantisAPI = "healthcheck" :> Get '[ JSON] HealthcheckResponse

getHealthcheck :: ClientM HealthcheckResponse
getHealthcheck = client (Proxy :: Proxy MantisAPI)

run :: Manager -> BaseUrl -> ClientM a -> IO (Either ServantError a)
run manager base action = runClientM action $ mkClientEnv manager base
