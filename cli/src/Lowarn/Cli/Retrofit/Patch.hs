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

withManifest :: Path Abs Dir -> String -> (Path Abs File -> IO a) -> IO a
withManifest manifestDirectory manifestName f =
  withSystemTempFile ("MANIFEST-" <> manifestName) $
    \manifestPath manifestHandle -> do
      (_, _, _, processHandle) <- withDevNull $ \errHandle ->
        createProcess $
          ( proc
              "rg"
              [ "--no-require-git",
                "--files",
                "--hidden",
                "--iglob",
                "!.git/",
                "--no-ignore-parent"
              ]
          )
            { cwd = Just $ toFilePath manifestDirectory,
              env = Just [],
              std_in = NoStream,
              std_out = UseHandle manifestHandle,
              std_err = UseHandle errHandle
            }
      waitForProcessFail processHandle $
        printf "Failed to generate manifest %s" manifestName
      f manifestPath

getPatchFilePath :: Path Abs Dir -> String -> IO (Path Abs File)
getPatchFilePath parentDirectory patchName = do
  relativePatchFilePath <- parseRelFile $ patchName <> ".patch"
  return $ parentDirectory </> relativePatchFilePath

checkDirectoryExists :: Path Abs Dir -> IO ()
checkDirectoryExists directory = do
  doesDirExist directory >>= \case
    True -> return ()
    False -> fail $ printf "Directory %s does not exist." $ toFilePath directory

makePatch :: Path Abs Dir -> Path Rel Dir -> Path Rel Dir -> String -> IO ()
makePatch parentDirectory oldDirectory newDirectory patchName = do
  checkDirectoryExists absoluteOldDirectory
  checkDirectoryExists absoluteNewDirectory

  patchFilePath <- getPatchFilePath parentDirectory patchName

  withManifest absoluteOldDirectory "OLD" $ \oldManifestPath ->
    withManifest absoluteNewDirectory "NEW" $ \newManifestPath -> do
      (pipeOut, pipeIn) <- createPipe
      (_, _, _, processHandle1) <-
        createProcess $
          ( proc
              "makepatch"
              [ toFilePath oldDirectory,
                toFilePath newDirectory,
                "-oldmanifest",
                toFilePath oldManifestPath,
                "-newmanifest",
                toFilePath newManifestPath,
                "-description",
                patchName,
                "-diff",
                "diff -u"
              ]
          )
            { env = Just [],
              cwd = Just $ toFilePath parentDirectory,
              std_in = NoStream,
              std_out = UseHandle pipeIn,
              std_err = Inherit
            }

      (_, _, _, processHandle2) <-
        withFile (toFilePath patchFilePath) WriteMode $ \outHandle ->
          withDevNull $ \errHandle ->
            createProcess $
              ( proc
                  "sed"
                  [ "-E",
                    "s/^(\\+\\+\\+.+|---.+|# Date generated +: |#### End of Patch kit \\[created: )[A-Z][a-z]{2} [A-Z][a-z]{2} [ 0-9][0-9] [0-9]{2}:[0-9]{2}:[0-9]{2} [0-9]{4}(\\] ####)?$/\\1Thu Jan 01 00:00:00 1970\\2/;/^#### (Patch c|C)hecksum: .+ ####$/d;s/^(# [cCrRp] '[^']+' [0-9]+ )[0-9]+(( 0)[0-9]+)?$/\\10\\3/"
                  ]
              )
                { env = Just [],
                  std_in = UseHandle pipeOut,
                  std_out = UseHandle outHandle,
                  std_err = UseHandle errHandle
                }

      waitForProcessFail processHandle1 $
        printf "Failed to generate patch %s" patchName
      waitForProcessFail processHandle2 $
        printf
          "Failed to replace dates and remove checksums in patch %s"
          patchName
  where
    absoluteOldDirectory = parentDirectory </> oldDirectory
    absoluteNewDirectory = parentDirectory </> newDirectory

applyPatch :: Path Abs Dir -> Path Rel Dir -> Path Rel Dir -> String -> IO ()
applyPatch parentDirectory oldDirectory newDirectory patchName = do
  checkDirectoryExists absoluteOldDirectory

  patchFilePath <- getPatchFilePath parentDirectory patchName
  doesFileExist patchFilePath >>= \case
    True -> return ()
    False -> fail $ printf "Patch %s does not exist." $ toFilePath patchFilePath

  doesDirExist absoluteNewDirectory >>= \case
    True -> removeDirRecur absoluteNewDirectory
    False -> return ()

  copyDirRecur absoluteOldDirectory absoluteNewDirectory

  (_, _, _, processHandle) <-
    createProcess $
      ( proc
          "applypatch"
          [ "--force",
            "-patch",
            "patch -p0 -N --merge",
            toFilePath patchFilePath
          ]
      )
        { env = Just [],
          cwd = Just $ toFilePath absoluteNewDirectory,
          std_in = NoStream,
          std_out = Inherit,
          std_err = Inherit
        }

  waitForProcess processHandle >>= \case
    ExitSuccess -> return ()
    ExitFailure errorCode ->
      putStrLn $ printf "applypatch ended with error code %d." errorCode
  where
    absoluteOldDirectory = parentDirectory </> oldDirectory
    absoluteNewDirectory = parentDirectory </> newDirectory

sourceDirectory, simplifiedDirectory, retrofittedDirectory :: Path Rel Dir
sourceDirectory = [reldir|source|]
simplifiedDirectory = [reldir|simplified|]
retrofittedDirectory = [reldir|retrofitted|]

makeSimplifyPatch :: Path Abs Dir -> IO ()
makeSimplifyPatch parentDirectory =
  makePatch
    parentDirectory
    sourceDirectory
    simplifiedDirectory
    "simplify"

makeRetrofitPatch :: Path Abs Dir -> IO ()
makeRetrofitPatch parentDirectory =
  makePatch
    parentDirectory
    simplifiedDirectory
    retrofittedDirectory
    "retrofit"

applySimplifyPatch :: Path Abs Dir -> IO ()
applySimplifyPatch parentDirectory =
  applyPatch
    parentDirectory
    sourceDirectory
    simplifiedDirectory
    "simplify"

applyRetrofitPatch :: Path Abs Dir -> IO ()
applyRetrofitPatch parentDirectory =
  applyPatch
    parentDirectory
    simplifiedDirectory
    retrofittedDirectory
    "retrofit"
