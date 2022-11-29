{-# LANGUAGE TemplateHaskellQuotes #-}

import Control.Monad (void)
import DsuTest (DsuTest, inputLine, outputLine, outputLines, runDsuTest, updateProgram)
import Lowarn.Runtime (Runtime, loadProgram)
import System.FilePath
  ( (<.>),
    (</>),
  )
import System.IO (Handle)
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )
import Test.Tasty.Golden
  ( goldenVsFileDiff,
  )

main :: IO ()
main = defaultMain =<< goldenTests

dsuGoldenTest ::
  String ->
  ((Handle, Handle) -> Runtime ()) ->
  DsuTest () ->
  Int ->
  TestTree
dsuGoldenTest testName getRuntime dsuTest timeout =
  goldenVsFileDiff
    testName
    (\a b -> ["diff", "-u", a, b])
    (testPath <.> "golden")
    (testPath <.> "log")
    $ runDsuTest dsuTest getRuntime (testPath <.> "log") timeout
  where
    testPath = "test" </> "golden" </> testName

simpleDsu :: TestTree
simpleDsu =
  dsuGoldenTest (show 'simpleDsu) getRuntime dsuTest 10000000
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

goldenTests :: IO TestTree
goldenTests = do
  return $
    testGroup
      "Simple DSU runtime golden tests"
      [simpleDsu]
