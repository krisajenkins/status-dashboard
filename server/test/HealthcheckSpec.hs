{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module HealthcheckSpec where

import Control.Lens ((^..), (^?), _Right, asIndex, ifolded)
import Data.Aeson (eitherDecode')
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Healthcheck (HealthcheckResponse)

spec :: Spec
spec =
  describe "JSON" $ do
    it "JSON decoding 1" $ do
      decoded :: Either String HealthcheckResponse <-
        eitherDecode' <$> LBS.readFile "test/healthcheck_good.json"
      decoded `shouldSatisfy` isRight
      -- decoded ^? (_Right . instructions . mainFilename) `shouldBe`
      --   Just "mortal.sol"
      -- decoded ^.. (_Right . instructions . files . ifolded . asIndex) `shouldBe`
      --   ["mortal.sol", "owned.sol"]
    it "JSON decoding 2" $ do
      decoded :: Either String HealthcheckResponse <-
        eitherDecode' <$> LBS.readFile "test/healthcheck_bad.json"
      decoded `shouldSatisfy` isRight
      -- decoded ^? (_Right . instructions . mainFilename) `shouldBe`
      --   Just "browser/ballot.sol"
      -- decoded ^.. (_Right . instructions . files . ifolded . asIndex) `shouldBe`
      --   ["browser/ballot.sol"]
