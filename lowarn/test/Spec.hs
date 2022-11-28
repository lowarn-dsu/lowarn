{-# LANGUAGE TemplateHaskellQuotes #-}

import Control.Monad (void)
import DsuTest (writeLine, readLines, runDsuTest, updateProgram)
import Lowarn.Runtime (loadProgram)
import System.FilePath
  ( (<.>),
    (</>),
  )
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )
import Test.Tasty.Golden
  ( goldenVsFile,
  )

main :: IO ()
main = defaultMain =<< goldenTests

simpleDsu :: TestTree
simpleDsu =
  goldenVsFile
    testName
    (testPath <.> "golden")
    (testPath <.> "txt")
    $ runDsuTest dsuTest getRuntime (testPath <.> "txt") 10000000
  where
    testName = show 'simpleDsu
    testPath = "test" </> "golden" </> show 'simpleDsu

    getRuntime handles =
      void $
        loadProgram
          "Lowarn.Programs.Program1"
          handles
          >>= loadProgram "Lowarn.Programs.Program2"
          >>= loadProgram "Lowarn.Programs.Program3"

    dsuTest = do
      _ <- readLines 3
      writeLine "A"
      _ <- readLines 4
      updateProgram
      writeLine "B"

      _ <- readLines 5
      writeLine "C"
      _ <- readLines 1
      writeLine "1234"
      _ <- readLines 6
      updateProgram
      writeLine "D"
      _ <- readLines 1
      writeLine "2345"

      _ <- readLines 7
      writeLine "E#3456"
      _ <- readLines 8
      updateProgram
      writeLine "F#4567"

goldenTests :: IO TestTree
goldenTests = do
  return $
    testGroup
      "Simple DSU runtime golden tests"
      [simpleDsu]
