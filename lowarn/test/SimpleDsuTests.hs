{-# LANGUAGE TemplateHaskellQuotes #-}

module SimpleDsuTests (goldenTests) where

import Control.Monad (void)
import DsuTest
  ( DsuTest,
    dsuGoldenTest,
    inputLine,
    outputLine,
    outputLines,
    runDsuTest,
    updateProgram,
  )
import Lowarn.Runtime (Runtime, loadProgram)
import Test.Tasty (TestTree, testGroup)

successfulChain :: TestTree
successfulChain =
  dsuGoldenTest (show 'successfulChain) getRuntime dsuTest 10000000
  where
    getRuntime handles =
      void $
        loadProgram
          "Lowarn.Programs.Program1"
          handles
          >>= loadProgram "Lowarn.Programs.Program2"
          >>= loadProgram "Lowarn.Programs.Program3"

    dsuTest = do
      _ <- outputLines 3
      inputLine "A"
      _ <- outputLines 4
      updateProgram
      inputLine "B"

      _ <- outputLines 5
      inputLine "C"
      _ <- outputLines 1
      inputLine "1234"
      _ <- outputLines 6
      updateProgram
      inputLine "D"
      _ <- outputLine
      inputLine "2345"

      _ <- outputLines 7
      inputLine "E#3456"
      _ <- outputLines 8
      updateProgram
      inputLine "F#4567"

goldenTests :: TestTree
goldenTests =
  testGroup
    "Simple DSU runtime golden tests"
    [successfulChain]
