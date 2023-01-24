{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.Transformer (transformerTests) where

import Test.Lowarn.Transformer (Expression (Expression), transformerGoldenTest)
import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ

reorderingVariant1Identity :: TestTree
reorderingVariant1Identity =
  transformerGoldenTest
    (show 'reorderingVariant1Identity)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Variant1 B.Variant1)
  A.Variant1A
|]

reorderingVariant1RenameConstructor :: TestTree
reorderingVariant1RenameConstructor =
  transformerGoldenTest
    (show 'reorderingVariant1RenameConstructor)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Variant1 C.Variant1)
  A.Variant1A
|]

reorderingVariant1AddedConstructor :: TestTree
reorderingVariant1AddedConstructor =
  transformerGoldenTest
    (show 'reorderingVariant1AddedConstructor)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Variant1 D.Variant1)
  A.Variant1A
|]

reorderingVariantTuple1Identity :: TestTree
reorderingVariantTuple1Identity =
  transformerGoldenTest
    (show 'reorderingVariantTuple1Identity)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.VariantTuple1 B.VariantTuple1)
  (A.VariantTuple1A 1)
|]

reorderingRecord1Identity :: TestTree
reorderingRecord1Identity =
  transformerGoldenTest
    (show 'reorderingRecord1Identity)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Record1 B.Record1)
  (A.Record1A 1)
|]

reorderingRecord1NewtypeToData :: TestTree
reorderingRecord1NewtypeToData =
  transformerGoldenTest
    (show 'reorderingRecord1NewtypeToData)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Record1' B.Record1')
  (A.Record1'A 1)
|]

reorderingRecord1RenameField :: TestTree
reorderingRecord1RenameField =
  transformerGoldenTest
    (show 'reorderingRecord1RenameField)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Record1 C.Record1)
  (A.Record1A 1)
|]

reorderingRecord1AddedField :: TestTree
reorderingRecord1AddedField =
  transformerGoldenTest
    (show 'reorderingRecord1AddedField)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Record1 D.Record1)
  (A.Record1A 1)
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

reorderingVariant2Swap :: TestTree
reorderingVariant2Swap =
  transformerGoldenTest
    (show 'reorderingVariant2Swap)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Variant2 C.Variant2)
  A.Variant2A
|]

reorderingRecord2Identity :: TestTree
reorderingRecord2Identity =
  transformerGoldenTest
    (show 'reorderingRecord2Identity)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Record2 B.Record2)
  (A.Record2A 1 "a")
|]

reorderingRecord2Swap :: TestTree
reorderingRecord2Swap =
  transformerGoldenTest
    (show 'reorderingRecord2Swap)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Record2 C.Record2)
  (A.Record2A 1 "a")
|]

reorderingVariant3Swap :: TestTree
reorderingVariant3Swap =
  transformerGoldenTest
    (show 'reorderingVariant3Swap)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Variant3 B.Variant3)
  A.Variant3A
|]

reorderingRecord3Swap :: TestTree
reorderingRecord3Swap =
  transformerGoldenTest
    (show 'reorderingRecord3Swap)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Record3 B.Record3)
  (A.Record3A 1 "a" True)
|]

reorderingVariantRecord3Swap :: TestTree
reorderingVariantRecord3Swap =
  transformerGoldenTest
    (show 'reorderingVariantRecord3Swap)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.VariantRecord3 B.VariantRecord3)
  (A.VariantRecord3B True 1 "a")
|]

transformerTests :: TestTree
transformerTests =
  testGroup
    "Transformers"
    [ reorderingVariant1Identity,
      reorderingVariant1RenameConstructor,
      reorderingVariant1AddedConstructor,
      reorderingVariantTuple1Identity,
      reorderingRecord1Identity,
      reorderingRecord1NewtypeToData,
      reorderingRecord1RenameField,
      reorderingRecord1AddedField,
      reorderingVariant2Identity,
      reorderingVariant2Swap,
      reorderingRecord2Identity,
      reorderingRecord2Swap,
      reorderingVariant3Swap,
      reorderingRecord3Swap,
      reorderingVariantRecord3Swap
    ]
