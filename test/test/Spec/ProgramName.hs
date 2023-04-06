{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Spec.ProgramName (programNameTests) where

import Data.Proxy
import Lowarn.ProgramName
import Lowarn.ProgramName.Aeson ()
import Lowarn.ProgramName.Arbitrary ()
import Test.Lowarn.Property
import Test.Tasty
import Test.Tasty.QuickCheck

programNameRoundTrip :: TestTree
programNameRoundTrip =
  testProperty (show 'programNameRoundTrip) $
    parserCombinatorRoundTripProperty unProgramName parseProgramName

prefixModuleNameRoundTrip :: TestTree
prefixModuleNameRoundTrip =
  testProperty (show 'prefixModuleNameRoundTrip) $
    parserCombinatorRoundTripProperty
      (showPrefixModuleName "EntryPoint")
      (parsePrefixModuleName "EntryPoint")

encodedProgramNameRoundTrip :: TestTree
encodedProgramNameRoundTrip =
  testProperty (show 'encodedProgramNameRoundTrip) $
    aesonRoundTripProperty (Proxy :: Proxy ProgramName)

programNameTests :: TestTree
programNameTests =
  testGroup
    "Program name"
    [ programNameRoundTrip,
      prefixModuleNameRoundTrip,
      encodedProgramNameRoundTrip
    ]
