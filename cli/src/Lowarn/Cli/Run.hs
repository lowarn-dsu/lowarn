{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module                  : Lowarn.Cli.Run
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for a command that allows Lowarn programs to be run.
module Lowarn.Cli.Run (run, runConfigRuntime, configRuntime) where

import Control.Monad.IO.Class
import Lowarn.Cli.Config
import Lowarn.Cli.Env
import Lowarn.Cli.VersionGraph
import Lowarn.Runtime
import Lowarn.UpdateId
import Lowarn.VersionId
import Lowarn.VersionNumber
import Path
import System.IO
import Text.Printf

-- | Find and run versions of the program, optionally starting with a given
-- version. The latest available version of the program is used when searching
-- for one.
run :: LowarnEnv -> Maybe VersionNumber -> IO ()
run LowarnEnv {..} mVersionNumber = do
  let LowarnConfig {..} = lowarnEnvConfig
  runConfigRuntime
    ( configRuntime
        lowarnEnvConfig
        ( getVersionGraph
            (parent lowarnEnvConfigPath)
            lowarnConfigProgramName
            lowarnConfigCabalDirectory
        )
        True
        (Left mVersionNumber)
    )
    lowarnEnvConfig
    >>= \case
      Left e ->
        hPutStrLn stderr $ printf "An error occurred in Lowarn's runtime:\n%s" e
      Right () -> return ()

-- | Run a runtime according to a CLI configuration.
runConfigRuntime :: Runtime a -> LowarnConfig -> IO a
runConfigRuntime runtime LowarnConfig {..} =
  runRuntime
    runtime
    lowarnConfigUnload
    lowarnConfigSystemLinker

-- | A Lowarn runtime that loads versions of a program according to a CLI
-- configuration file, an action that gets the version graph, and a boolean that
-- indicates whether latest or earliest versions should be used.
configRuntime ::
  -- | A CLI configuration.
  LowarnConfig ->
  -- | An action that gets the version graph.
  IO VersionGraph ->
  -- | A boolean that is @True@ if the latest versions of programs should be
  -- used by the runtime, and @False@ if the earliest versions should be used.
  Bool ->
  -- | Either @Left mNextVersionNumber@, where @mNextVersionNumber@ is the first
  -- version of the program to load (the latest version is used if
  -- @mNextVersionNumber@ is @Nothing@), or
  -- @Right (previousVersionNumber, previousState)@, where
  -- @previousVersionNumber@ is the version number of the version of the program
  -- to update from and @previousState@ is its state.
  Either (Maybe VersionNumber) (VersionNumber, a) ->
  Runtime (Either String ())
configRuntime
  lowarnConfig@(LowarnConfig {..})
  getVersionGraphAction
  shouldPreferLatest
  ePreviousVersionNumberAndState = do
    updateRuntimePackageDatabase
    versionGraph <- liftIO getVersionGraphAction
    eNextVersionNumberAndState <- case ePreviousVersionNumberAndState of
      Left mNextVersionNumber -> do
        case maybe (eGetVersionNumber versionGraph) Right mNextVersionNumber of
          Left e -> return $ Left e
          Right nextVersionNumber ->
            Right . (nextVersionNumber,)
              <$> loadVersion
                (VersionId lowarnConfigProgramName nextVersionNumber)
                Nothing
      Right (previousVersionNumber, previousState) ->
        case getNextVersionNumber previousVersionNumber versionGraph of
          Just nextVersionNumber ->
            Right . (nextVersionNumber,)
              <$> loadUpdate
                ( UpdateId
                    lowarnConfigProgramName
                    previousVersionNumber
                    nextVersionNumber
                )
                previousState
          Nothing ->
            return
              $ Left
              $ printf
                "Next version could not be loaded as there are no updates in the version graph from version %s."
              $ showWithDots previousVersionNumber
    case eNextVersionNumberAndState of
      Left e -> return $ Left e
      Right (nextVersionNumber, nextState) ->
        configRuntime lowarnConfig getVersionGraphAction shouldPreferLatest $
          Right (nextVersionNumber, nextState)
    where
      (getVersionNumber, getNextVersionNumber) =
        if shouldPreferLatest
          then (latestVersionNumber, latestNextVersionNumber)
          else (earliestVersionNumber, earliestNextVersionNumber)

      eGetVersionNumber :: VersionGraph -> Either String VersionNumber
      eGetVersionNumber =
        maybe
          (Left "Latest version could not be loaded as there are no versions in the version graph.")
          Right
          . getVersionNumber
