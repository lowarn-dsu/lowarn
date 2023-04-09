{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.Story (storyTests) where

import Control.Monad
import Lowarn.ExampleProgram.ManualFollowing.DemoInfo
import Lowarn.Runtime
import System.IO
import Test.Lowarn.Story
import Test.Lowarn.Tasty
import Test.Tasty

getExampleRuntime :: (Handle, Handle) -> Runtime ()
getExampleRuntime fileHandles =
  void $ loadUpdate manualFollowingUpdateId_0_1 fileHandles

timeout :: Int
timeout = 40000000

inputTimeout :: IO BinarySemaphore -> TestTree
inputTimeout =
  storyGoldenTest
    (show 'inputTimeout)
    getExampleRuntime
    defaultRunRuntime
    (void $ outputLines 3)
    timeout

outputTimeout :: IO BinarySemaphore -> TestTree
outputTimeout =
  storyGoldenTest
    (show 'outputTimeout)
    getExampleRuntime
    defaultRunRuntime
    (void $ outputLines 7)
    timeout

pipeOrderingWithInputFirst :: IO BinarySemaphore -> TestTree
pipeOrderingWithInputFirst =
  storyGoldenTest
    (show 'pipeOrderingWithInputFirst)
    getExampleRuntime
    defaultRunRuntime
    (void $ inputLine "A" >> outputLines 7)
    timeout

pipeOrderingWithOutputFirst :: IO BinarySemaphore -> TestTree
pipeOrderingWithOutputFirst =
  storyGoldenTest
    (show 'pipeOrderingWithOutputFirst)
    getExampleRuntime
    defaultRunRuntime
    (outputLines 7 >> inputLine "A")
    timeout

info :: IO BinarySemaphore -> TestTree
info =
  storyGoldenTest
    (show 'info)
    (const $ return ())
    defaultRunRuntime
    (writeInfo "Test")
    timeout

storyTests :: IO BinarySemaphore -> TestTree
storyTests binarySemaphoreAction =
  testGroup
    "Story framework"
    $ [ inputTimeout,
        outputTimeout,
        pipeOrderingWithInputFirst,
        pipeOrderingWithOutputFirst,
        info
      ]
      <*> [binarySemaphoreAction]
