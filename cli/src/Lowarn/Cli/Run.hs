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

run :: LowarnEnv -> IO ()
run lowarnEnv =
  runRuntime (runWithState lowarnEnv Nothing) True >>= \case
    Left e ->
      hPutStrLn stderr $ printf "An error occurred in Lowarn's runtime:\n%s" e
    Right () -> return ()

runWithState ::
  LowarnEnv -> Maybe (VersionNumber, a) -> Runtime (Either String ())
runWithState
  lowarnEnv@LowarnEnv {lowarnEnvConfig = LowarnConfig {..}, ..}
  mPreviousVersionNumberAndState = do
    updateRuntimePackageDatabase
    versionGraph <-
      liftIO $
        getVersionGraph (parent lowarnEnvConfigPath) lowarnConfigProgramName
    eNextVersionNumberAndState <- case mPreviousVersionNumberAndState of
      Nothing ->
        return $ case latestVersionNumber versionGraph of
          Just nextVersionNumber -> Right (nextVersionNumber, Nothing)
          Nothing ->
            Left
              "Latest version could not be loaded as there are no versions in the version graph."
      Just (previousVersionNumber, previousState) ->
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
      Right (nextVersionNumber, mState) ->
        runWithState lowarnEnv . Just . (nextVersionNumber,)
          =<< loadVersion
            (VersionId lowarnConfigProgramName nextVersionNumber)
            mState
