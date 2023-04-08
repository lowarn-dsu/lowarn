{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.ManualDsu (manualDsuTests) where

import Control.Monad
import Lowarn.ExampleProgram.Following.DemoInfo
import Lowarn.ExampleProgram.ManualFollowing.DemoInfo
import Lowarn.Runtime
import Lowarn.UpdateId
import System.IO
import Test.Lowarn.Story
import Test.Lowarn.Tasty
import Test.Tasty

getExampleRuntime ::
  UpdateId ->
  UpdateId ->
  UpdateId ->
  (Handle, Handle) ->
  Runtime ()
getExampleRuntime
  updateId_0_1
  updateId_1_2
  updateId_2_3
  fileHandles =
    void $
      loadUpdate updateId_0_1 fileHandles
        >>= loadUpdate updateId_1_2
        >>= loadUpdate updateId_2_3

getExampleManualFollowingRuntime :: (Handle, Handle) -> Runtime ()
getExampleManualFollowingRuntime =
  getExampleRuntime
    manualFollowingUpdateId_0_1
    manualFollowingUpdateId_1_2
    manualFollowingUpdateId_2_3

getExampleFollowingRuntime :: (Handle, Handle) -> Runtime ()
getExampleFollowingRuntime =
  getExampleRuntime
    followingUpdateId_0_1
    followingUpdateId_1_2
    followingUpdateId_2_3

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
