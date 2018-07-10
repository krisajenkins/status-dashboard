{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Webserver.Types
  ( OverallStatus(..)
  ) where

import Data.Aeson
  ( FromJSON
  , ToJSON
  , (.:)
  , (.:?)
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , genericToJSON
  , object
  , parseJSON
  , toJSON
  , withArray
  , withObject
  )
import Data.Aeson.Types (Parser)
import GHC.Generics (Generic)

data OverallStatus
  = Good
  | Bad
  | Unknown
  deriving (Show, Eq, Generic)

instance ToJSON OverallStatus where
  toJSON status = object [("status", genericToJSON defaultOptions status)]
