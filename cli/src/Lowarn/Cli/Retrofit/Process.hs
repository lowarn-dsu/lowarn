{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.Cli.Retrofit.Process
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for running processes for Lowarn CLI retrofit.
module Lowarn.Cli.Retrofit.Process
  ( gitDirProc,
    waitForProcessFail,
    withDevNull,
  )
where

import Path
import System.Exit
import System.IO
import System.Process
import Text.Printf

-- | 'Process.proc' that runs in the @repo@ subdirectory of a directory given by
-- 'withRetrofitDirectory'.
gitDirProc :: Path Abs Dir -> FilePath -> [String] -> CreateProcess
gitDirProc retrofitDirectory command arguments =
  (proc command arguments)
    { cwd = Just $ toFilePath $ retrofitDirectory </> [reldir|repo|],
      env = Just []
    }

-- | Wait for a process to end and print a given error message if it fails.
waitForProcessFail :: ProcessHandle -> String -> IO ()
waitForProcessFail processHandle failMessage =
  waitForProcess processHandle >>= \case
    ExitSuccess -> return ()
    ExitFailure errorCode ->
      fail $ printf "%s (error code %d)." failMessage errorCode

-- | Run an action with a handle that writes to @/dev/null@.
withDevNull :: (Handle -> IO a) -> IO a
withDevNull = withFile "/dev/null" WriteMode
