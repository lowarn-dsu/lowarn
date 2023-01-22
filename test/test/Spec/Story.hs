{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.Story (storyTests) where

import Control.Monad (void)
import Lowarn.ExampleProgram.ManualFollowing.TransformerId
  ( manualFollowingTransformerId_0_1,
  )
import Lowarn.ExampleProgram.ManualFollowing.VersionId
  ( manualFollowingVersionId_1,
  )
import Lowarn.Runtime (Runtime, loadTransformer, loadVersion)
import System.IO (Handle)
import Test.Lowarn.Story
  ( inputLine,
    outputLines,
    storyGoldenTest,
    writeInfo,
  )
import Test.Lowarn.Tasty (BinarySemaphore)
import Test.Tasty (TestTree, testGroup)

getExampleRuntime :: (Handle, Handle) -> Runtime ()
getExampleRuntime handles =
  void $
    loadTransformer manualFollowingTransformerId_0_1 handles
      >>= loadVersion manualFollowingVersionId_1

timeout :: Int
timeout = 40000000

inputTimeout :: IO BinarySemaphore -> TestTree
inputTimeout =
  storyGoldenTest
    (show 'inputTimeout)
    getExampleRuntime
    (void $ outputLines 3)
    timeout

outputTimeout :: IO BinarySemaphore -> TestTree
outputTimeout =
  storyGoldenTest
    (show 'outputTimeout)
    getExampleRuntime
    (void $ outputLines 7)
    timeout

pipeOrderingWithInputFirst :: IO BinarySemaphore -> TestTree
pipeOrderingWithInputFirst =
  storyGoldenTest
    (show 'pipeOrderingWithInputFirst)
    getExampleRuntime
    (void $ inputLine "A" >> outputLines 7)
    timeout

pipeOrderingWithOutputFirst :: IO BinarySemaphore -> TestTree
pipeOrderingWithOutputFirst =
  storyGoldenTest
    (show 'pipeOrderingWithOutputFirst)
    getExampleRuntime
    (outputLines 7 >> inputLine "A")
    timeout

info :: IO BinarySemaphore -> TestTree
info =
  storyGoldenTest
    (show 'info)
    (const $ return ())
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
