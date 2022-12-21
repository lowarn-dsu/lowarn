{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.ProgramName (programNameTests) where

import Lowarn.ProgramName
  ( parseEntryPointModuleName,
    parseProgramName,
    parseTransformerModuleName,
    showEntryPointModuleName,
    showTransformerModuleName,
    unProgramName,
  )
import Lowarn.ProgramName.Arbitrary ()
import Test.Lowarn.Property (roundTripProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

programNameRoundTrip :: TestTree
programNameRoundTrip =
  testProperty (show 'programNameRoundTrip) $
    roundTripProperty unProgramName parseProgramName

entryPointModuleNameRoundTrip :: TestTree
entryPointModuleNameRoundTrip =
  testProperty (show 'entryPointModuleNameRoundTrip) $
    roundTripProperty showEntryPointModuleName parseEntryPointModuleName

transformerModuleNameRoundTrip :: TestTree
transformerModuleNameRoundTrip =
  testProperty (show 'transformerModuleNameRoundTrip) $
    roundTripProperty showTransformerModuleName parseTransformerModuleName

programNameTests :: TestTree
programNameTests =
  testGroup
    "Program name tests"
    [ programNameRoundTrip,
      entryPointModuleNameRoundTrip,
      transformerModuleNameRoundTrip
    ]
