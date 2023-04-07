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

jsonUpdateIdRoundTrip :: TestTree
jsonUpdateIdRoundTrip =
  testProperty (show 'jsonUpdateIdRoundTrip) $
    jsonRoundTripProperty (Proxy :: Proxy UpdateId)

yamlUpdateIdRoundTrip :: TestTree
yamlUpdateIdRoundTrip =
  testProperty (show 'yamlUpdateIdRoundTrip) $
    yamlRoundTripProperty (Proxy :: Proxy UpdateId)

updateIdTests :: TestTree
updateIdTests =
  testGroup
    "Update ID"
    [ updateIdRoundTrip,
      updatePackageNameRoundTrip,
      jsonUpdateIdRoundTrip,
      yamlUpdateIdRoundTrip
    ]
