{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.Dsu (dsuTests) where

import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Lowarn.Cli.Config
import Lowarn.Cli.Run
import Lowarn.Cli.VersionGraph
import Lowarn.ExampleProgram.CustomFfi.DemoInfo
import Lowarn.ExampleProgram.Following.DemoInfo
import Lowarn.ExampleProgram.ManualFollowing.DemoInfo
import Lowarn.ParserCombinators
import Lowarn.Runtime
import Lowarn.UpdateId
import Lowarn.VersionNumber
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

getExampleManualFollowingRuntime,
  getExampleFollowingRuntime ::
    (Handle, Handle) -> Runtime ()
getExampleManualFollowingRuntime =
  getExampleRuntime
    manualFollowingUpdateId_0_1
    manualFollowingUpdateId_1_2
    manualFollowingUpdateId_2_3
getExampleFollowingRuntime =
  getExampleRuntime
    followingUpdateId_0_1
    followingUpdateId_1_2
    followingUpdateId_2_3

mkVersionNumberFromString :: String -> VersionNumber
mkVersionNumberFromString = fromJust . readWithParser parseWithDots

versionNumber0, versionNumber1, versionNumber2, versionNumber3 :: VersionNumber
versionNumber0 = mkVersionNumberFromString "0"
versionNumber1 = mkVersionNumberFromString "1.0.0"
versionNumber2 = mkVersionNumberFromString "2.0.0"
versionNumber3 = mkVersionNumberFromString "3.0.0"

exampleVersionGraph :: VersionGraph
exampleVersionGraph =
  VersionGraph $
    Map.fromAscList
      [ (versionNumber0, Set.singleton versionNumber1),
        (versionNumber1, Set.singleton versionNumber2),
        (versionNumber2, Set.singleton versionNumber3),
        (versionNumber3, Set.empty)
      ]

getConfigRuntime :: LowarnConfig -> (Handle, Handle) -> Runtime ()
getConfigRuntime config handles =
  void $
    configRuntime
      config
      (return exampleVersionGraph)
      False
      (Right (versionNumber0, handles))

manualFollowingConfig, followingConfig :: LowarnConfig
manualFollowingConfig =
  LowarnConfig manualFollowingProgramName False False Nothing
followingConfig = LowarnConfig followingProgramName True True Nothing

getExampleRuntimes ::
  [(String, (Handle, Handle) -> Runtime (), Runtime () -> IO ())]
getExampleRuntimes =
  [ ("manualFollowing", getExampleManualFollowingRuntime, defaultRunRuntime),
    ("following", getExampleFollowingRuntime, defaultRunRuntime),
    ( "configManualFollowing",
      getConfigRuntime manualFollowingConfig,
      flip runConfigRuntime manualFollowingConfig
    ),
    ( "configFollowing",
      getConfigRuntime followingConfig,
      flip runConfigRuntime followingConfig
    )
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
      ( \(exampleRuntimeName, getRuntime, runRuntimePreset) ->
          storyGoldenTest
            (testName <> ('.' : exampleRuntimeName))
            getRuntime
            runRuntimePreset
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
    dsuTest :: Story ()
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
    dsuTest :: Story ()
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

customFfi :: IO BinarySemaphore -> TestTree
customFfi =
  storyGoldenTest
    (show 'customFfi)
    ( \(_, outHandle) ->
        void $ loadVersion customFfiVersionId_1 $ Just outHandle
    )
    defaultRunRuntime
    (void $ outputLines 4)
    timeout

dsuTests :: IO BinarySemaphore -> TestTree
dsuTests binarySemaphoreAction =
  testGroup
    "DSU"
    $ [ successfulChain,
        duplicatedUpdateSignal,
        customFfi
      ]
      <*> [binarySemaphoreAction]
