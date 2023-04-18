{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.Cli.Retrofit.Patch
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for Lowarn CLI retrofit patches.
module Lowarn.Cli.Retrofit.Patch
  ( -- * General
    makePatch,
    applyPatch,

    -- * Simplify
    makeSimplifyPatch,
    applySimplifyPatch,

    -- * Retrofit
    makeRetrofitPatch,
    applyRetrofitPatch,
  )
where

import Lowarn.Cli.Retrofit.Process
import Path
import Path.IO
import System.Exit
import System.IO
import System.Process
import Text.Printf

copyNonIgnoredToDirectory :: Path Abs Dir -> Path Abs Dir -> IO ()
copyNonIgnoredToDirectory fromDirectory toDirectory = do
  createDirIfMissing True toDirectory
  (pipeOut, pipeIn) <- createPipe
  (_, _, _, processHandle1) <- withDevNull $ \errHandle ->
    createProcess $
      ( proc
          "rg"
          [ "--no-require-git",
            "--files",
            "--hidden",
            "--iglob",
            "!.git/",
            "--iglob",
            "!*.orig",
            "--no-ignore-parent"
          ]
      )
        { cwd = Just $ toFilePath fromDirectory,
          env = Just [],
          std_in = NoStream,
          std_out = UseHandle pipeIn,
          std_err = UseHandle errHandle
        }

  (_, _, _, processHandle2) <- withDevNull $ \errHandle ->
    withDevNull $ \outHandle ->
      createProcess $
        ( proc
            "xargs"
            ["cp", "--parents", "-t", toFilePath toDirectory]
        )
          { cwd = Just $ toFilePath fromDirectory,
            env = Just [],
            std_in = UseHandle pipeOut,
            std_out = UseHandle outHandle,
            std_err = UseHandle errHandle
          }

  waitForProcessFail processHandle1 $
    printf "Failed to find files to copy in %s" $
      toFilePath fromDirectory
  waitForProcessFail processHandle2 $
    printf
      "Failed to copy files from %s to %s"
      (toFilePath fromDirectory)
      (toFilePath fromDirectory)

getPatchFilePath :: Path Abs Dir -> String -> IO (Path Abs File)
getPatchFilePath parentDirectory patchName = do
  relativePatchFilePath <- parseRelFile $ patchName <> ".patch"
  return $ parentDirectory </> relativePatchFilePath

checkDirectoryExists :: Path Abs Dir -> IO ()
checkDirectoryExists directory = do
  doesDirExist directory >>= \case
    True -> return ()
    False -> fail $ printf "Directory %s does not exist." $ toFilePath directory

withDevNullOrInherit :: Bool -> (StdStream -> IO a) -> IO a
withDevNullOrInherit False f = f Inherit
withDevNullOrInherit True f = withDevNull $ f . UseHandle

-- | Create a patch file between two similar subdirectories of a directory. The
-- patch file is placed in the parent directory and given the name
-- @patch-name.patch@, where @patch-name@ is the given patch name.
--
-- The patch file is created using @git --no-index@. @rg@ (ripgrep) is used to
-- ignore files that are ignored by any @.gitignore@ or @.ignore@ files in each
-- subdirectory, in the same way that Git does. @.git@ directories and @.orig@
-- files are also ignored.
makePatch ::
  -- | The parent directory of the two subdirectories.
  Path Abs Dir ->
  -- | The subdirectory to create the patch from.
  Path Rel Dir ->
  -- | The subdirectory to create the patch to.
  Path Rel Dir ->
  -- | The patch name.
  String ->
  -- | Whether or not to suppress command output.
  Bool ->
  IO ()
makePatch parentDirectory oldDirectory newDirectory patchName silent = do
  checkDirectoryExists absoluteOldDirectory
  checkDirectoryExists absoluteNewDirectory

  patchFilePath <- getPatchFilePath parentDirectory patchName

  withSystemTempDir "patch" $ \patchDirectory -> do
    copyNonIgnoredToDirectory
      absoluteOldDirectory
      (patchDirectory </> oldDirectory)
    copyNonIgnoredToDirectory
      absoluteNewDirectory
      (patchDirectory </> newDirectory)

    (_, _, _, processHandle1) <-
      withDevNullOrInherit silent $ \errStream ->
        withFile (toFilePath patchFilePath) WriteMode $ \outHandle ->
          createProcess $
            ( proc
                "git"
                [ "diff",
                  "--no-index",
                  "--minimal",
                  toFilePath oldDirectory,
                  toFilePath newDirectory
                ]
            )
              { env = Just [],
                cwd = Just $ toFilePath patchDirectory,
                std_in = NoStream,
                std_out = UseHandle outHandle,
                std_err = errStream
              }

    waitForProcess processHandle1 >>= \case
      ExitSuccess -> return ()
      ExitFailure 1 -> return ()
      ExitFailure errorCode ->
        printf
          "Failed to generate patch %s (error code %d)."
          patchName
          errorCode
  where
    absoluteOldDirectory = parentDirectory </> oldDirectory
    absoluteNewDirectory = parentDirectory </> newDirectory

-- | Apply a patch created by 'makePatch' to a subdirectory of a directory to
-- create a new subdirectory with the patch applied. If the new subdirectory
-- already exists, it is replaced. We merge the patch, so this may result in
-- merge conflict markers in the new subdirectory.
applyPatch ::
  -- | The parent directory of the two subdirectories.
  Path Abs Dir ->
  -- | The subdirectory to apply the patch from.
  Path Rel Dir ->
  -- | The result subdirectory.
  Path Rel Dir ->
  -- | The patch name.
  String ->
  -- | Whether or not to suppress command output.
  Bool ->
  IO ()
applyPatch parentDirectory oldDirectory newDirectory patchName silent = do
  checkDirectoryExists absoluteOldDirectory

  patchFilePath <- getPatchFilePath parentDirectory patchName
  doesFileExist patchFilePath >>= \case
    True -> return ()
    False -> fail $ printf "Patch %s does not exist." $ toFilePath patchFilePath

  doesDirExist absoluteNewDirectory >>= \case
    True -> removeDirRecur absoluteNewDirectory
    False -> return ()

  copyNonIgnoredToDirectory absoluteOldDirectory absoluteNewDirectory

  (_, _, _, processHandle) <-
    withDevNullOrInherit silent $ \outStream ->
      withDevNullOrInherit silent $ \errStream ->
        createProcess $
          ( proc
              "patch"
              [ "-p2",
                "-N",
                "--merge",
                "-E",
                "-i",
                toFilePath patchFilePath
              ]
          )
            { env = Just [],
              cwd = Just $ toFilePath absoluteNewDirectory,
              std_in = NoStream,
              std_out = outStream,
              std_err = errStream
            }

  waitForProcess processHandle >>= \case
    ExitFailure errorCode
      | not silent ->
          putStrLn $ printf "patch ended with error code %d." errorCode
    _ -> return ()
  where
    absoluteOldDirectory = parentDirectory </> oldDirectory
    absoluteNewDirectory = parentDirectory </> newDirectory

sourceDirectory, simplifiedDirectory, retrofittedDirectory :: Path Rel Dir
sourceDirectory = [reldir|source|]
simplifiedDirectory = [reldir|simplified|]
retrofittedDirectory = [reldir|retrofitted|]

-- | Run 'makePatch' from the @source@ subdirectory to the @simplified@
-- subdirectory, creating a patch file @simplify.patch@.
makeSimplifyPatch ::
  -- | The version directory.
  Path Abs Dir ->
  -- | Whether or not to suppress command output.
  Bool ->
  IO ()
makeSimplifyPatch parentDirectory =
  makePatch
    parentDirectory
    sourceDirectory
    simplifiedDirectory
    "simplify"

-- | Run 'makePatch' from the @simplified@ subdirectory to the @retrofitted@
-- subdirectory, creating a patch file @retrofit.patch@.
makeRetrofitPatch ::
  -- | The version directory.
  Path Abs Dir ->
  -- | Whether or not to suppress command output.
  Bool ->
  IO ()
makeRetrofitPatch parentDirectory =
  makePatch
    parentDirectory
    simplifiedDirectory
    retrofittedDirectory
    "retrofit"

-- | Run 'applyPatch' from the @source@ subdirectory to the @simplified@
-- subdirectory, using the patch file @simplify.patch@.
applySimplifyPatch ::
  -- | The version directory.
  Path Abs Dir ->
  -- | Whether or not to suppress command output.
  Bool ->
  IO ()
applySimplifyPatch parentDirectory =
  applyPatch
    parentDirectory
    sourceDirectory
    simplifiedDirectory
    "simplify"

-- | Run 'applyPatch' from the @simplified@ subdirectory to the @retrofitted@
-- subdirectory, using the patch file @retrofit.patch@.
applyRetrofitPatch ::
  -- | The version directory.
  Path Abs Dir ->
  -- | Whether or not to suppress command output.
  Bool ->
  IO ()
applyRetrofitPatch parentDirectory =
  applyPatch
    parentDirectory
    simplifiedDirectory
    retrofittedDirectory
    "retrofit"
