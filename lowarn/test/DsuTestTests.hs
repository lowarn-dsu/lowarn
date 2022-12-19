{-# LANGUAGE TemplateHaskellQuotes #-}

module DsuTestTests (goldenTests) where

import Control.Monad (void)
import DsuTest
  ( dsuGoldenTest,
    inputLine,
    outputLines,
    writeInfo,
  )
import Lowarn.Runtime (Runtime, loadTransformer, loadVersion)
import System.IO (Handle)
import Test.Tasty (TestTree, testGroup)
import Following
  ( followingTransformerId,
    followingVersionId,
    versionNumber0,
    versionNumber1,
  )

getExampleRuntime :: (Handle, Handle) -> Runtime ()
getExampleRuntime handles =
  void $
    loadTransformer
      (followingTransformerId (versionNumber0, versionNumber1))
      handles
      >>= loadVersion (followingVersionId versionNumber1)

inputTimeout :: TestTree
inputTimeout =
  dsuGoldenTest
    (show 'inputTimeout)
    getExampleRuntime
    (void $ outputLines 3)
    3000000

outputTimeout :: TestTree
outputTimeout =
  dsuGoldenTest
    (show 'outputTimeout)
    getExampleRuntime
    (void $ outputLines 7)
    3000000

pipeOrderingWithInputFirst :: TestTree
pipeOrderingWithInputFirst =
  dsuGoldenTest
    (show 'pipeOrderingWithInputFirst)
    getExampleRuntime
    (void $ inputLine "A" >> outputLines 7)
    3000000

pipeOrderingWithOutputFirst :: TestTree
pipeOrderingWithOutputFirst =
  dsuGoldenTest
    (show 'pipeOrderingWithOutputFirst)
    getExampleRuntime
    (outputLines 7 >> inputLine "A")
    3000000

info :: TestTree
info =
  dsuGoldenTest
    (show 'info)
    (const $ return ())
    (writeInfo "Test")
    3000000

goldenTests :: TestTree
goldenTests =
  testGroup
    "DSU testing framework golden tests"
    [ inputTimeout,
      outputTimeout,
      pipeOrderingWithInputFirst,
      pipeOrderingWithOutputFirst,
      info
    ]
