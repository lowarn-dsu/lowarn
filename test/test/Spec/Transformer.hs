{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.Transformer (transformerTests) where

import Test.Lowarn.Transformer (Expression (Expression), transformerGoldenTest)
import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ

reorderingVariant1 :: TestTree
reorderingVariant1 =
  transformerGoldenTest
    (show 'reorderingVariant1)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Variant1 B.Variant1)
  A.Variant1
|]

reorderingRecord1 :: TestTree
reorderingRecord1 =
  transformerGoldenTest
    (show 'reorderingRecord1)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Record1 B.Record1)
  (A.Record1 1)
|]

transformerTests :: TestTree
transformerTests =
  testGroup
    "Transformers"
    [reorderingVariant1
    , reorderingRecord1
    ]
