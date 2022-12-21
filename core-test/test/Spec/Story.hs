{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.Story (storyTests) where

import Control.Monad (void)
import Lowarn.ExampleProgram.Following.TransformerId (followingTransformerId_0_1)
import Lowarn.ExampleProgram.Following.VersionId (followingVersionId_1)
import Lowarn.Runtime (Runtime, loadTransformer, loadVersion)
import System.IO (Handle)
import Test.Lowarn.Story
  ( inputLine,
    outputLines,
    storyGoldenTest,
    writeInfo,
  )
import Test.Tasty (TestTree, testGroup)

getExampleRuntime :: (Handle, Handle) -> Runtime ()
getExampleRuntime handles =
  void $
    loadTransformer followingTransformerId_0_1 handles
      >>= loadVersion followingVersionId_1

inputTimeout :: TestTree
inputTimeout =
  storyGoldenTest
    (show 'inputTimeout)
    getExampleRuntime
    (void $ outputLines 3)
    3000000

outputTimeout :: TestTree
outputTimeout =
  storyGoldenTest
    (show 'outputTimeout)
    getExampleRuntime
    (void $ outputLines 7)
    3000000

pipeOrderingWithInputFirst :: TestTree
pipeOrderingWithInputFirst =
  storyGoldenTest
    (show 'pipeOrderingWithInputFirst)
    getExampleRuntime
    (void $ inputLine "A" >> outputLines 7)
    3000000

pipeOrderingWithOutputFirst :: TestTree
pipeOrderingWithOutputFirst =
  storyGoldenTest
    (show 'pipeOrderingWithOutputFirst)
    getExampleRuntime
    (outputLines 7 >> inputLine "A")
    3000000

info :: TestTree
info =
  storyGoldenTest
    (show 'info)
    (const $ return ())
    (writeInfo "Test")
    3000000

storyTests :: TestTree
storyTests =
  testGroup
    "Story framework"
    [ inputTimeout,
      outputTimeout,
      pipeOrderingWithInputFirst,
      pipeOrderingWithOutputFirst,
      info
    ]
