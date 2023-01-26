{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.Transformer (transformerTests) where

import Test.Lowarn.Transformer
  ( Expression (Expression),
    Import (Import),
    transformerGoldenTest,
  )
import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ

failImports :: [Import]
failImports = [Import "Test.Lowarn.Type.Instance.Fail"]

datatypeNameAliasImport,
  constructorNameAliasImport,
  fieldNameAliasImport ::
    Import
datatypeNameAliasImport =
  Import "Test.Lowarn.Type.Instance.DatatypeNameAlias"
constructorNameAliasImport =
  Import "Test.Lowarn.Type.Instance.ConstructorNameAlias"
fieldNameAliasImport =
  Import "Test.Lowarn.Type.Instance.FieldNameAlias"

aliasImports :: [Import]
aliasImports =
  [ datatypeNameAliasImport,
    constructorNameAliasImport,
    fieldNameAliasImport
  ]

identity :: TestTree
identity =
  transformerGoldenTest
    (show 'identity)
    []
    $ Expression
      [r|
unTransformer
  (transformer :: Transformer Int Int)
  0
|]

traversable :: TestTree
traversable =
  transformerGoldenTest
    (show 'traversable)
    []
    $ Expression
      [r|
unTransformer
  (traversableTransformer :: Transformer [A.Record1] [B.Record1])
  [A.Record1A 0, A.Record1A 1, A.Record1A 2]
|]

traversableFail :: TestTree
traversableFail =
  transformerGoldenTest
    (show 'traversableFail)
    failImports
    $ Expression
      [r|
unTransformer
  (traversableTransformer :: Transformer [A.Variant2] [B.Variant2])
  [A.Variant2A, A.Variant2B, A.Variant2A]
|]

genericVariantUnlabelled :: TestTree
genericVariantUnlabelled =
  transformerGoldenTest
    (show 'genericVariantUnlabelled)
    []
    $ Expression
      [r|
unTransformer
  (genericTransformer :: Transformer A.VariantUnlabelled3 A.VariantUnlabelled3')
  (A.VariantUnlabelled3A 1 "a" True)
|]

genericVariantRecord :: TestTree
genericVariantRecord =
  transformerGoldenTest
    (show 'genericVariantRecord)
    []
    $ Expression
      [r|
unTransformer
  (genericTransformer :: Transformer A.VariantRecord3 A.VariantRecord3')
  (A.VariantRecord3A 1 "a" True)
|]

genericFail :: TestTree
genericFail =
  transformerGoldenTest
    (show 'genericFail)
    [Import "Test.Lowarn.Type.Instance.Fail"]
    $ Expression
      [r|
unTransformer
  (genericTransformer :: Transformer A.Variant1Wrapper B.Variant1Wrapper)
  (A.Variant1Wrapper A.Variant1A)
|]

genericVariantSwap :: TestTree
genericVariantSwap =
  transformerGoldenTest
    (show 'genericVariantSwap)
    []
    $ Expression
      [r|
unTransformer
  (genericTransformer :: Transformer A.Variant2 C.Variant2)
  A.Variant2A
|]

genericRecordSwap :: TestTree
genericRecordSwap =
  transformerGoldenTest
    (show 'genericRecordSwap)
    []
    $ Expression
      [r|
unTransformer
  (genericTransformer :: Transformer A.Record2 C.Record2)
  (A.Record2A 1 "a")
|]

genericUnlabelledToRecord :: TestTree
genericUnlabelledToRecord =
  transformerGoldenTest
    (show 'genericUnlabelledToRecord)
    []
    $ Expression
      [r|
unTransformer
  (genericTransformer :: Transformer A.VariantUnlabelled3 A.VariantRecord3)
  (A.VariantUnlabelled3A 1 "a" True)
|]

genericRecordToUnlabelled :: TestTree
genericRecordToUnlabelled =
  transformerGoldenTest
    (show 'genericRecordToUnlabelled)
    []
    $ Expression
      [r|
unTransformer
  (genericTransformer :: Transformer A.VariantRecord3 A.VariantUnlabelled3)
  (A.VariantRecord3A 1 "a" True)
|]

renamingIdentity :: TestTree
renamingIdentity =
  transformerGoldenTest
    (show 'renamingIdentity)
    []
    $ Expression
      [r|
unTransformer
  (genericRenamingTransformer :: Transformer A.Record2 B.Record2)
  (A.Record2A 1 "a")
|]

renamingRenameVariantUnlabelled :: TestTree
renamingRenameVariantUnlabelled =
  transformerGoldenTest
    (show 'renamingRenameVariantUnlabelled)
    aliasImports
    $ Expression
      [r|
unTransformer
  (genericRenamingTransformer :: Transformer A.VariantUnlabelled3 A.VariantUnlabelled3')
  (A.VariantUnlabelled3A 1 "a" True)
|]

renamingRenameVariantRecord :: TestTree
renamingRenameVariantRecord =
  transformerGoldenTest
    (show 'renamingRenameVariantRecord)
    aliasImports
    $ Expression
      [r|
unTransformer
  (genericRenamingTransformer :: Transformer A.VariantRecord3 A.VariantRecord3')
  (A.VariantRecord3A 1 "a" True)
|]

renamingWithoutDatatypeNameAliases :: TestTree
renamingWithoutDatatypeNameAliases =
  transformerGoldenTest
    (show 'renamingWithoutDatatypeNameAliases)
    [constructorNameAliasImport, fieldNameAliasImport]
    $ Expression
      [r|
unTransformer
  (genericRenamingTransformer :: Transformer A.VariantRecord3 A.VariantRecord3')
  (A.VariantRecord3A 1 "a" True)
|]

renamingWithoutConstructorNameAliases :: TestTree
renamingWithoutConstructorNameAliases =
  transformerGoldenTest
    (show 'renamingWithoutConstructorNameAliases)
    [datatypeNameAliasImport, fieldNameAliasImport]
    $ Expression
      [r|
unTransformer
  (genericRenamingTransformer :: Transformer A.VariantRecord3 A.VariantRecord3')
  (A.VariantRecord3A 1 "a" True)
|]

renamingWithoutFieldNameAliases :: TestTree
renamingWithoutFieldNameAliases =
  transformerGoldenTest
    (show 'renamingWithoutFieldNameAliases)
    [datatypeNameAliasImport, constructorNameAliasImport]
    $ Expression
      [r|
unTransformer
  (genericRenamingTransformer :: Transformer A.VariantRecord3 A.VariantRecord3')
  (A.VariantRecord3A 1 "a" True)
|]

renamingVariantSwap :: TestTree
renamingVariantSwap =
  transformerGoldenTest
    (show 'renamingVariantSwap)
    aliasImports
    $ Expression
      [r|
unTransformer
  (genericRenamingTransformer :: Transformer A.Variant2 C.Variant2)
  A.Variant2A
|]

renamingRecordSwap :: TestTree
renamingRecordSwap =
  transformerGoldenTest
    (show 'renamingRecordSwap)
    aliasImports
    $ Expression
      [r|
unTransformer
  (genericRenamingTransformer :: Transformer A.Record2 C.Record2)
  (A.Record2A 1 "a")
|]

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

reorderingVariant1AddConstructor :: TestTree
reorderingVariant1AddConstructor =
  transformerGoldenTest
    (show 'reorderingVariant1AddConstructor)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Variant1 D.Variant1)
  A.Variant1A
|]

reorderingVariant1RenameType :: TestTree
reorderingVariant1RenameType =
  transformerGoldenTest
    (show 'reorderingVariant1RenameType)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Variant1 E.Variant1')
  A.Variant1A
|]

reorderingUnlabelled1Identity :: TestTree
reorderingUnlabelled1Identity =
  transformerGoldenTest
    (show 'reorderingUnlabelled1Identity)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Unlabelled1 B.Unlabelled1)
  (A.Unlabelled1A 1)
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

reorderingRecord1AddField :: TestTree
reorderingRecord1AddField =
  transformerGoldenTest
    (show 'reorderingRecord1AddField)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Record1 D.Record1)
  (A.Record1A 1)
|]

reorderingRecord1Newtype :: TestTree
reorderingRecord1Newtype =
  transformerGoldenTest
    (show 'reorderingRecord1Newtype)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Record1' B.Record1')
  (A.Record1'A 1)
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

reorderingUnlabelled2Identity :: TestTree
reorderingUnlabelled2Identity =
  transformerGoldenTest
    (show 'reorderingUnlabelled2Identity)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Unlabelled2 B.Unlabelled2)
  (A.Unlabelled2A 1 "a")
|]

reorderingUnlabelled2Swap :: TestTree
reorderingUnlabelled2Swap =
  transformerGoldenTest
    (show 'reorderingUnlabelled2Swap)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Unlabelled2 C.Unlabelled2)
  (A.Unlabelled2A 1 "a")
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

reorderingRecord2Rename :: TestTree
reorderingRecord2Rename =
  transformerGoldenTest
    (show 'reorderingRecord2Rename)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.Record2 D.Record2)
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

reorderingVariantUnlabelled3SwapConstructors :: TestTree
reorderingVariantUnlabelled3SwapConstructors =
  transformerGoldenTest
    (show 'reorderingVariantUnlabelled3SwapConstructors)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.VariantUnlabelled3 B.VariantUnlabelled3)
  (A.VariantUnlabelled3B True 1 "a")
|]

reorderingVariantUnlabelled3Swap :: TestTree
reorderingVariantUnlabelled3Swap =
  transformerGoldenTest
    (show 'reorderingVariantUnlabelled3Swap)
    []
    $ Expression
      [r|
unTransformer
  (genericReorderingTransformer :: Transformer A.VariantUnlabelled3 C.VariantUnlabelled3)
  (A.VariantUnlabelled3B True 1 "a")
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

unwrapData :: TestTree
unwrapData =
  transformerGoldenTest
    (show 'unwrapData)
    []
    $ Expression
      [r|
unTransformer
  (genericUnwrapTransformer :: Transformer A.Variant1Wrapper A.Variant1)
  (A.Variant1Wrapper A.Variant1A)
|]

unwrapNewtype :: TestTree
unwrapNewtype =
  transformerGoldenTest
    (show 'unwrapNewtype)
    []
    $ Expression
      [r|
unTransformer
  (genericUnwrapTransformer :: Transformer A.Variant1Wrapper' A.Variant1)
  (A.Variant1Wrapper' A.Variant1A)
|]

unwrapMultiple :: TestTree
unwrapMultiple =
  transformerGoldenTest
    (show 'unwrapMultiple)
    []
    $ Expression
      [r|
unTransformer
  (genericUnwrapTransformer :: Transformer A.Variant1Wrapper'' A.Variant1)
  (A.Variant1Wrapper'' A.Variant1A A.Variant1A)
|]

wrapData :: TestTree
wrapData =
  transformerGoldenTest
    (show 'wrapData)
    []
    $ Expression
      [r|
unTransformer
  (genericWrapTransformer :: Transformer A.Variant1 A.Variant1Wrapper)
  A.Variant1A
|]

wrapNewtype :: TestTree
wrapNewtype =
  transformerGoldenTest
    (show 'wrapNewtype)
    []
    $ Expression
      [r|
unTransformer
  (genericWrapTransformer :: Transformer A.Variant1 A.Variant1Wrapper')
  A.Variant1A
|]

coerceData :: TestTree
coerceData =
  transformerGoldenTest
    (show 'coerceData)
    []
    $ Expression
      [r|
unTransformer
  (coerceTransformer :: Transformer A.Variant1 A.Variant1Wrapper)
  A.Variant1A
|]

coerceNewtype :: TestTree
coerceNewtype =
  transformerGoldenTest
    (show 'coerceNewtype)
    []
    $ Expression
      [r|
unTransformer
  (coerceTransformer :: Transformer A.Variant1 A.Variant1Wrapper')
  A.Variant1A
|]

transformerTests :: TestTree
transformerTests =
  testGroup
    "Transformers"
    [ identity,
      traversable,
      traversableFail,
      genericVariantUnlabelled,
      genericVariantRecord,
      genericFail,
      genericVariantSwap,
      genericRecordSwap,
      genericUnlabelledToRecord,
      genericRecordToUnlabelled,
      renamingIdentity,
      renamingRenameVariantUnlabelled,
      renamingRenameVariantRecord,
      renamingWithoutDatatypeNameAliases,
      renamingWithoutConstructorNameAliases,
      renamingWithoutFieldNameAliases,
      renamingVariantSwap,
      renamingRecordSwap,
      reorderingVariant1Identity,
      reorderingVariant1RenameConstructor,
      reorderingVariant1AddConstructor,
      reorderingVariant1RenameType,
      reorderingUnlabelled1Identity,
      reorderingRecord1Identity,
      reorderingRecord1RenameField,
      reorderingRecord1AddField,
      reorderingRecord1Newtype,
      reorderingVariant2Identity,
      reorderingVariant2Swap,
      reorderingUnlabelled2Identity,
      reorderingUnlabelled2Swap,
      reorderingRecord2Identity,
      reorderingRecord2Swap,
      reorderingRecord2Rename,
      reorderingVariant3Swap,
      reorderingRecord3Swap,
      reorderingVariantUnlabelled3SwapConstructors,
      reorderingVariantUnlabelled3Swap,
      reorderingVariantRecord3Swap,
      unwrapData,
      unwrapNewtype,
      unwrapMultiple,
      wrapData,
      wrapNewtype,
      coerceData,
      coerceNewtype
    ]
