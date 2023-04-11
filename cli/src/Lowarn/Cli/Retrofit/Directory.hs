{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.Cli.Retrofit.Directory
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for the internal directory used by Lowarn CLI retrofit.
module Lowarn.Cli.Retrofit.Directory
  ( withRetrofitDirectory,
    getRetrofitHash,
    cloneInto,
    writeCommitMap,
    clean,
    copyCommitState,
  )
where

import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Lowarn.Cli.Config
import Lowarn.Cli.Env
import Lowarn.Cli.Retrofit.BranchName
import Path
import Path.IO
import System.Exit
import System.IO
import System.Process
import Text.Hex (encodeHex)
import Text.Printf
import URI.ByteString

-- | Give the SHA256 hash associated with repository information found inside a
-- 'LowarnRetrofitConfig'.
getRetrofitHash :: LowarnRetrofitConfig -> String
getRetrofitHash LowarnRetrofitConfig {..} =
  Text.unpack $
    encodeHex $
      SHA256.finalize $
        SHA256.updates
          SHA256.init
          [ serializeURIRef' lowarnRetrofitConfigGitUri,
            Text.encodeUtf8 $
              Text.pack $
                unBranchName lowarnRetrofitConfigBranch
          ]

-- | Clone a repository specified by a 'LowarnRetrofitConfig' into a @repo@
-- subdirectory of a given directory.
cloneInto :: LowarnRetrofitConfig -> Path Abs Dir -> IO ()
cloneInto LowarnRetrofitConfig {..} retrofitDirectory = do
  (exitCode, _, errors) <-
    readProcessWithExitCode
      "git"
      [ "clone",
        "-b",
        unBranchName lowarnRetrofitConfigBranch,
        uriString,
        toFilePath $ retrofitDirectory </> [reldir|repo|]
      ]
      ""

  case exitCode of
    ExitSuccess -> return ()
    ExitFailure errorCode ->
      fail $
        unlines
          [ printf
              "Could not clone repository %s with branch %s (error code %d)."
              uriString
              (unBranchName lowarnRetrofitConfigBranch)
              errorCode,
            "Git errors:",
            errors
          ]
  where
    uriString =
      Text.unpack $
        Text.decodeUtf8 $
          serializeURIRef' lowarnRetrofitConfigGitUri

gitDirProc :: Path Abs Dir -> FilePath -> [String] -> CreateProcess
gitDirProc retrofitDirectory command arguments =
  (proc command arguments)
    { cwd = Just $ toFilePath $ retrofitDirectory </> [reldir|repo|],
      env = Just []
    }

waitForProcessFail :: ProcessHandle -> String -> IO ()
waitForProcessFail processHandle failMessage =
  waitForProcess processHandle >>= \case
    ExitSuccess -> return ()
    ExitFailure errorCode ->
      fail $ printf "%s (error code %d)." failMessage errorCode

-- | Write a list of commit hashes in chronological order to a @commit-map@ file
-- in a given directory, using a Git repository found in a @repo@ subdirectory.
-- The commits are ordered from oldest to newest by taking the first parent of
-- each commit from the head of the repository's current branch.
writeCommitMap :: Path Abs Dir -> IO ()
writeCommitMap retrofitDirectory = do
  (_, _, _, processHandle) <-
    withFile "/dev/null" WriteMode $ \errHandle ->
      withFile
        (toFilePath $ retrofitDirectory </> [relfile|commit-map|])
        WriteMode
        $ \outHandle ->
          createProcess $
            ( gitDirProc
                retrofitDirectory
                "git"
                [ "log",
                  "--first-parent",
                  "--pretty=format:%H",
                  "--reverse",
                  "HEAD"
                ]
            )
              { std_in = NoStream,
                std_out = UseHandle outHandle,
                std_err = UseHandle errHandle
              }

  waitForProcessFail processHandle "Could not generate commit map"

-- | Run an action with a directory containing a Git repository and commit map
-- file generated from a 'LowarnEnv'.
--
-- The path of this directory is @config-path/.lowarn-retrofit/hash@, where
-- @config-path@ is the path of the directory containing the Lowarn
-- configuration file and @hash@ is the SHA256 hash of the retrofit config
-- created with 'getRetrofitHash'. If any directories other than @hash@ exist in
-- @config-path/.lowarn-retrofit@, they are removed.
withRetrofitDirectory :: LowarnEnv -> (Path Abs Dir -> IO a) -> IO a
withRetrofitDirectory LowarnEnv {..} f = do
  let LowarnConfig {..} = lowarnEnvConfig
  retrofitConfig <-
    maybe
      (fail "No retrofit configuration found.")
      return
      lowarnConfigRetrofitConfig
  retrofitHashDirectory <- parseRelDir $ getRetrofitHash retrofitConfig
  let retrofitDirectoryParent =
        parent lowarnEnvConfigPath </> [reldir|.lowarn-retrofit|]
      retrofitDirectory = retrofitDirectoryParent </> retrofitHashDirectory

  createDirIfMissing True retrofitDirectory
  (subdirectories, _) <- listDirRel retrofitDirectoryParent
  mapM_
    ( \subdirectory ->
        when (subdirectory /= retrofitHashDirectory) $
          removeDirRecur $
            retrofitDirectoryParent </> subdirectory
    )
    subdirectories

  doesDirExist (retrofitDirectory </> [reldir|repo|]) >>= \case
    True -> return ()
    False -> cloneInto retrofitConfig retrofitDirectory
  doesFileExist (retrofitDirectory </> [relfile|commit-map|]) >>= \case
    True -> return ()
    False -> writeCommitMap retrofitDirectory

  f retrofitDirectory

-- | Remove any @.lowarn-retrofit@ directory in the parent directory of a given
-- file path.
clean :: Path Abs File -> IO ()
clean lowarnConfigPath =
  doesDirExist retrofitDirectory >>= \case
    True -> removeDirRecur retrofitDirectory
    False -> return ()
  where
    retrofitDirectory = parent lowarnConfigPath </> [reldir|.lowarn-retrofit|]

-- | Copy the state of a repository at a given commit to a given directory.
--
-- This action changes the HEAD of the repository.
copyCommitState ::
  -- | The internal Lowarn CLI retrofit directory.
  Path Abs Dir ->
  -- | The destination directory to copy the commit state to. This directory is
  -- created if it does not already exist.
  Path Abs Dir ->
  -- | The commit ID.
  String ->
  IO ()
copyCommitState retrofitDirectory destination commitId = do
  (exitCode, _, errors) <-
    readCreateProcessWithExitCode
      (gitDirProc retrofitDirectory "git" ["reset", "--hard", commitId])
      ""
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure errorCode ->
      fail $
        unlines
          [ printf
              "Could not reset repository to commit %s (error code %d)."
              commitId
              errorCode,
            "Git errors:",
            errors
          ]

  doesDirExist destination
    >>= \case
      True -> removeDirRecur destination
      False -> return ()
  createDirIfMissing True destination

  (pipeOut, pipeIn) <- createPipe
  (_, _, _, processHandle1) <- withFile "/dev/null" WriteMode $ \errHandle ->
    createProcess $
      (gitDirProc retrofitDirectory "git" ["ls-files"])
        { std_in = NoStream,
          std_out = UseHandle pipeIn,
          std_err = UseHandle errHandle
        }

  (_, _, _, processHandle2) <- withFile "/dev/null" WriteMode $ \errHandle ->
    withFile "/dev/null" WriteMode $ \outHandle ->
      createProcess $
        ( gitDirProc
            retrofitDirectory
            "xargs"
            ["cp", "--parents", "-t", toFilePath destination]
        )
          { std_in = UseHandle pipeOut,
            std_out = UseHandle outHandle,
            std_err = UseHandle errHandle
          }

  waitForProcessFail processHandle1 "Could not get repository files"
  waitForProcessFail processHandle2 "Could not copy repository files"
