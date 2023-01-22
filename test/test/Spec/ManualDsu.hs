{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.ManualDsu (manualDsuTests) where

import Control.Monad (void)
import Lowarn.ExampleProgram.Following.TransformerId
import Lowarn.ExampleProgram.Following.VersionId
import Lowarn.ExampleProgram.ManualFollowing.TransformerId
import Lowarn.ExampleProgram.ManualFollowing.VersionId
import Lowarn.Runtime (Runtime, loadTransformer, loadVersion)
import Lowarn.TransformerId (TransformerId)
import Lowarn.VersionId (VersionId)
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
  VersionId ->
  TransformerId ->
  VersionId ->
  TransformerId ->
  VersionId ->
  (Handle, Handle) ->
  Runtime ()
getExampleRuntime
  transformerId_0_1
  versionId_1
  transformerId_1_2
  versionId_2
  transformerId_2_3
  versionId_3
  handles =
    void $
      loadTransformer transformerId_0_1 handles
        >>= loadVersion versionId_1
        >>= loadTransformer transformerId_1_2
        >>= loadVersion versionId_2
        >>= loadTransformer transformerId_2_3
        >>= loadVersion versionId_3

getExampleManualFollowingRuntime :: (Handle, Handle) -> Runtime ()
getExampleManualFollowingRuntime =
  getExampleRuntime
    manualFollowingTransformerId_0_1
    manualFollowingVersionId_1
    manualFollowingTransformerId_1_2
    manualFollowingVersionId_2
    manualFollowingTransformerId_2_3
    manualFollowingVersionId_3

getExampleFollowingRuntime :: (Handle, Handle) -> Runtime ()
getExampleFollowingRuntime =
  getExampleRuntime
    followingTransformerId_0_1
    followingVersionId_1
    followingTransformerId_1_2
    followingVersionId_2
    followingTransformerId_2_3
    followingVersionId_3

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
