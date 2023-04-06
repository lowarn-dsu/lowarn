{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.UpdateId (updateIdTests) where

import Data.Proxy
import Lowarn.UpdateId
import Lowarn.UpdateId.Aeson ()
import Lowarn.UpdateId.Arbitrary ()
import Test.Lowarn.Property
import Test.Tasty
import Test.Tasty.QuickCheck

updateIdRoundTrip :: TestTree
updateIdRoundTrip =
  testProperty (show 'updateIdRoundTrip) $
    parserCombinatorRoundTripProperty showUpdateId parseUpdateId

updatePackageNameRoundTrip :: TestTree
updatePackageNameRoundTrip =
  testProperty (show 'updatePackageNameRoundTrip) $
    parserCombinatorRoundTripProperty
      showUpdatePackageName
      parseUpdatePackageName

encodedUpdateIdRoundTrip :: TestTree
encodedUpdateIdRoundTrip =
  testProperty (show 'encodedUpdateIdRoundTrip) $
    aesonRoundTripProperty (Proxy :: Proxy UpdateId)

updateIdTests :: TestTree
updateIdTests =
  testGroup
    "Update ID"
    [ updateIdRoundTrip,
      updatePackageNameRoundTrip,
      encodedUpdateIdRoundTrip
    ]
