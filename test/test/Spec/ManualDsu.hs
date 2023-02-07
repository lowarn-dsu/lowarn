{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.ManualDsu (manualDsuTests) where

import Control.Monad (void)
import Lowarn.ExampleProgram.Following.TransformerId
import Lowarn.ExampleProgram.ManualFollowing.TransformerId
import Lowarn.Runtime (Runtime, loadTransformerAndVersion)
import Lowarn.TransformerId (TransformerId)
import System.IO (Handle)
import Test.Lowarn.Story
  ( Story,
    inputLine,
    outputLine,
    outputLines,
    storyGoldenTest,
    updateProgram,
  )
import Test.Lowarn.Tasty (BinarySemaphore)
import Test.Tasty (TestTree, testGroup)

getExampleRuntime ::
  TransformerId ->
  TransformerId ->
  TransformerId ->
  (Handle, Handle) ->
  Runtime ()
getExampleRuntime
  transformerId_0_1
  transformerId_1_2
  transformerId_2_3
  handles =
    void $
      loadTransformerAndVersion transformerId_0_1 handles
        >>= loadTransformerAndVersion transformerId_1_2
        >>= loadTransformerAndVersion transformerId_2_3

getExampleManualFollowingRuntime :: (Handle, Handle) -> Runtime ()
getExampleManualFollowingRuntime =
  getExampleRuntime
    manualFollowingTransformerId_0_1
    manualFollowingTransformerId_1_2
    manualFollowingTransformerId_2_3

getExampleFollowingRuntime :: (Handle, Handle) -> Runtime ()
getExampleFollowingRuntime =
  getExampleRuntime
    followingTransformerId_0_1
    followingTransformerId_1_2
    followingTransformerId_2_3

getExampleRuntimes :: [(String, (Handle, Handle) -> Runtime ())]
getExampleRuntimes =
  [ ("manualFollowing", getExampleManualFollowingRuntime),
    ("following", getExampleFollowingRuntime)
  ]

storyGoldenTests ::
  String ->
  Story () ->
  Int ->
  IO BinarySemaphore ->
  TestTree
storyGoldenTests testName story timeout' binarySemaphoreAction =
  testGroup testName $
    map
      ( \(exampleRuntimeName, getExampleRuntime') ->
          storyGoldenTest
            (testName <> ('.' : exampleRuntimeName))
            getExampleRuntime'
            story
            timeout'
            binarySemaphoreAction
      )
      getExampleRuntimes

timeout :: Int
timeout = 60000000

successfulChain :: IO BinarySemaphore -> TestTree
successfulChain =
  storyGoldenTests
    (show 'successfulChain)
    dsuTest
    timeout
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

duplicatedUpdateSignal :: IO BinarySemaphore -> TestTree
duplicatedUpdateSignal =
  storyGoldenTests
    (show 'duplicatedUpdateSignal)
    dsuTest
    timeout
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

manualDsuTests :: IO BinarySemaphore -> TestTree
manualDsuTests binarySemaphoreAction =
  testGroup
    "Manual DSU runtime"
    $ [ successfulChain,
        duplicatedUpdateSignal
      ]
      <*> [binarySemaphoreAction]
