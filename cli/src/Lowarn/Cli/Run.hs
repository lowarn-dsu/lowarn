{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module                  : Lowarn.Cli.Run
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (GHC)
--
-- Module for a command that allows Lowarn programs to be run.
module Lowarn.Cli.Run (run) where

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
run lowarnEnv@LowarnEnv {lowarnEnvConfig = LowarnConfig {..}} mVersionNumber =
  runRuntime
    (runWithState lowarnEnv $ Left mVersionNumber)
    lowarnConfigLazyUpdates
    True
    >>= \case
      Left e ->
        hPutStrLn stderr $ printf "An error occurred in Lowarn's runtime:\n%s" e
      Right () -> return ()

runWithState ::
  LowarnEnv ->
  Either (Maybe VersionNumber) (VersionNumber, a) ->
  Runtime (Either String ())
runWithState
  lowarnEnv@LowarnEnv {lowarnEnvConfig = LowarnConfig {..}, ..}
  ePreviousVersionNumberAndState = do
    updateRuntimePackageDatabase
    versionGraph <-
      liftIO $
        getVersionGraph (parent lowarnEnvConfigPath) lowarnConfigProgramName
    eNextVersionNumberAndState <- case ePreviousVersionNumberAndState of
      Left mNextVersionNumber -> do
        case maybe
          ( maybe
              (Left "Latest version could not be loaded as there are no versions in the version graph.")
              Right
              $ latestVersionNumber versionGraph
          )
          Right
          mNextVersionNumber of
          Left e -> return $ Left e
          Right nextVersionNumber ->
            Right . (nextVersionNumber,)
              <$> loadVersion
                (VersionId lowarnConfigProgramName nextVersionNumber)
                Nothing
      Right (previousVersionNumber, previousState) ->
        case latestNextVersionNumber previousVersionNumber versionGraph of
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
        runWithState lowarnEnv $ Right (nextVersionNumber, nextState)
