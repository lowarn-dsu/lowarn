{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.ProgramName (programNameTests) where

import Lowarn.ProgramName
import Lowarn.ProgramName.Arbitrary ()
import Test.Lowarn.Property
import Test.Tasty
import Test.Tasty.QuickCheck

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
