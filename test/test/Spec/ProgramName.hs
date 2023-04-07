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

jsonProgramNameRoundTrip :: TestTree
jsonProgramNameRoundTrip =
  testProperty (show 'jsonProgramNameRoundTrip) $
    jsonRoundTripProperty (Proxy :: Proxy ProgramName)

yamlProgramNameRoundTrip :: TestTree
yamlProgramNameRoundTrip =
  testProperty (show 'yamlProgramNameRoundTrip) $
    yamlRoundTripProperty (Proxy :: Proxy ProgramName)

programNameTests :: TestTree
programNameTests =
  testGroup
    "Program name"
    [ programNameRoundTrip,
      prefixModuleNameRoundTrip,
      jsonProgramNameRoundTrip,
      yamlProgramNameRoundTrip
    ]
