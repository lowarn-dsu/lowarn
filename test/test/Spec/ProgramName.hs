{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.ProgramName (programNameTests) where

import Lowarn.ProgramName
  ( parseEntryPointModuleName,
    parseProgramName,
    parseUpdateModuleName,
    showEntryPointModuleName,
    showUpdateModuleName,
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

updateModuleNameRoundTrip :: TestTree
updateModuleNameRoundTrip =
  testProperty (show 'updateModuleNameRoundTrip) $
    roundTripProperty showUpdateModuleName parseUpdateModuleName

programNameTests :: TestTree
programNameTests =
  testGroup
    "Program name"
    [ programNameRoundTrip,
      entryPointModuleNameRoundTrip,
      updateModuleNameRoundTrip
    ]
