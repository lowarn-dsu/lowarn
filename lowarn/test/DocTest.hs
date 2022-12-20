{-# LANGUAGE LambdaCase #-}

module DocTest
  ( docTest,
  )
where

import Data.Functor ((<&>))
import Data.Typeable (Typeable)
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import System.Posix (forkProcess, getProcessStatus)
import System.Posix.Process (ProcessStatus (Exited, Stopped, Terminated))
import Test.DocTest
import Test.Tasty (TestTree)
import Test.Tasty.Providers (IsTest (..), singleTest, testFailed, testPassed)
import Text.Printf (printf)

data DocTest = DocTest deriving (Typeable)

instance IsTest DocTest where
  run _ _ _ = do
    processId <- forkProcess $ doctest ["-isrc", "src/Lowarn/ProgramName.hs"]
    getProcessStatus True True processId
      <&> \case
        Just (Exited ExitSuccess) -> testPassed ""
        Nothing -> testFailed "DocTest process stopped."
        Just (Exited (ExitFailure exitCode)) ->
          testFailed $ printf "DocTest process exited with code %d." exitCode
        Just (Terminated _ _) -> testFailed "DocTest process terminated."
        Just (Stopped _) -> testFailed "DocTest process stopped."

  testOptions = return []

docTest :: TestTree
docTest =
  singleTest "DocTest" DocTest
