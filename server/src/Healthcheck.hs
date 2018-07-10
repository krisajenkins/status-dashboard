{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Healthcheck where

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
import Data.Text (Text)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

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
