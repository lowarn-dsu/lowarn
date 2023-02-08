{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.UpdateId (updateIdTests) where

import Lowarn.UpdateId
  ( parseUpdateId,
    parseUpdatePackageName,
    showUpdateId,
    showUpdatePackageName,
  )
import Lowarn.UpdateId.Arbitrary ()
import Test.Lowarn.Property (roundTripProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

updateIdRoundTrip :: TestTree
updateIdRoundTrip =
  testProperty (show 'updateIdRoundTrip) $
    roundTripProperty showUpdateId parseUpdateId

updatePackageNameRoundTrip :: TestTree
updatePackageNameRoundTrip =
  testProperty (show 'updatePackageNameRoundTrip) $
    roundTripProperty showUpdatePackageName parseUpdatePackageName

updateIdTests :: TestTree
updateIdTests =
  testGroup
    "Update ID"
    [ updateIdRoundTrip,
      updatePackageNameRoundTrip
    ]
