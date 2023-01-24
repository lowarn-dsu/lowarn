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

reorderingVariantTuple1 :: TestTree
reorderingVariantTuple1 =
  transformerGoldenTest
    (show 'reorderingVariantTuple1)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.VariantTuple1 B.VariantTuple1)
  (A.VariantTuple1 1)
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

reorderingVariant2Identity :: TestTree
reorderingVariant2Identity =
  transformerGoldenTest
    (show 'reorderingVariant2Identity)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Variant2 B.Variant2)
  A.Variant2A
|]

transformerTests :: TestTree
transformerTests =
  testGroup
    "Transformers"
    [ reorderingVariant1,
      reorderingVariantTuple1,
      reorderingRecord1,
      reorderingVariant2Identity
    ]
