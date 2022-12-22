{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.TransformerId (transformerIdTests) where

import Lowarn.TransformerId
  ( parseTransformerId,
    parseTransformerPackageName,
    showTransformerId,
    showTransformerPackageName,
  )
import Lowarn.TransformerId.Arbitrary ()
import Test.Lowarn.Property (roundTripProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

transformerIdRoundTrip :: TestTree
transformerIdRoundTrip =
  testProperty (show 'transformerIdRoundTrip) $
    roundTripProperty showTransformerId parseTransformerId

transformerPackageNameRoundTrip :: TestTree
transformerPackageNameRoundTrip =
  testProperty (show 'transformerPackageNameRoundTrip) $
    roundTripProperty showTransformerPackageName parseTransformerPackageName

transformerIdTests :: TestTree
transformerIdTests =
  testGroup
    "Transformer ID"
    [ transformerIdRoundTrip,
      transformerPackageNameRoundTrip
    ]
