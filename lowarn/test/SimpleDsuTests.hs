{-# LANGUAGE TemplateHaskellQuotes #-}

module SimpleDsuTests (goldenTests) where

import Control.Monad (void)
import DsuTest
  ( dsuGoldenTest,
    inputLine,
    outputLine,
    outputLines,
    updateProgram,
  )
import Following
  ( followingTransformerId,
    followingVersionId,
    versionNumber0,
    versionNumber1,
    versionNumber2,
    versionNumber3,
  )
import Lowarn.Runtime (Runtime, loadTransformer, loadVersion)
import System.IO (Handle)
import Test.Tasty (TestTree, testGroup)

getExampleRuntime :: (Handle, Handle) -> Runtime ()
getExampleRuntime handles =
  void $
    loadTransformer
      (followingTransformerId (versionNumber0, versionNumber1))
      handles
      >>= loadVersion (followingVersionId versionNumber1)
      >>= loadTransformer
        (followingTransformerId (versionNumber1, versionNumber2))
      >>= loadVersion (followingVersionId versionNumber2)
      >>= loadTransformer
        (followingTransformerId (versionNumber2, versionNumber3))
      >>= loadVersion (followingVersionId versionNumber3)

successfulChain :: TestTree
successfulChain =
  dsuGoldenTest
    (show 'successfulChain)
    getExampleRuntime
    dsuTest
    10000000
  where
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

duplicatedUpdateSignal :: TestTree
duplicatedUpdateSignal =
  dsuGoldenTest
    (show 'duplicatedUpdateSignal)
    getExampleRuntime
    dsuTest
    10000000
  where
    dsuTest = do
      _ <- outputLines 3
      inputLine "A"
      _ <- outputLines 4
      updateProgram
      updateProgram
      updateProgram
      inputLine "B"

      _ <- outputLines 5
      inputLine "C"
      _ <- outputLines 1
      inputLine "1234"
      _ <- outputLines 6
      return ()

goldenTests :: TestTree
goldenTests =
  testGroup
    "Simple DSU runtime golden tests"
    [ successfulChain,
      duplicatedUpdateSignal
    ]
