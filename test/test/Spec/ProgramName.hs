{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.ProgramName (programNameTests) where

import Lowarn.ProgramName
  ( parsePrefixModuleName,
    parseProgramName,
    showPrefixModuleName,
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

prefixModuleNameRoundTrip :: TestTree
prefixModuleNameRoundTrip =
  testProperty (show 'prefixModuleNameRoundTrip) $
    roundTripProperty
      (showPrefixModuleName "EntryPoint")
      (parsePrefixModuleName "EntryPoint")

programNameTests :: TestTree
programNameTests =
  testGroup
    "Program name"
    [ programNameRoundTrip,
      prefixModuleNameRoundTrip
    ]
