{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.Cli.Run
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (GHC)
--
-- Module for a command that allows Lowarn programs to be run.
module Lowarn.Cli.Run (run) where

import Lowarn.Cli.Config (LowarnConfig (lowarnConfigProgramName))
import Lowarn.Cli.Env
import Lowarn.Cli.VersionGraph
import Path

run :: LowarnEnv -> IO ()
run LowarnEnv {..} = do
  versionGraph <-
    getVersionGraph
      (parent lowarnEnvConfigPath)
      (lowarnConfigProgramName lowarnEnvConfig)
  return ()
