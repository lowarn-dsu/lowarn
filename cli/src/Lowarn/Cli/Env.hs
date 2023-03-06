{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.Cli.Env
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (GHC)
--
-- Module for Lowarn CLI environments.
module Lowarn.Cli.Env (LowarnEnv (..), GetEnvException, getLowarnEnv) where

import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Yaml
import Lowarn.Cli.Config
import Path
import Path.IO
import Text.Printf

-- | Type for the environment used by functions in Lowarn's CLI.
data LowarnEnv = LowarnEnv
  { -- | The configuration.
    lowarnEnvConfig :: LowarnConfig,
    -- | The path of the configuration file.
    lowarnEnvConfigPath :: Path Abs File
  }
  deriving (Show)

-- | Type for the exceptions raised while creating 'LowarnEnv'.
data GetEnvException
  = ConfigParseException ParseException
  | PathException PathException
  deriving (Show)

instance Exception GetEnvException where
  displayException :: GetEnvException -> String
  displayException e =
    printf
      "Could not run Lowarn CLI due to a %s error:\n%s"
      errorTypeString
      errorString
    where
      (errorTypeString, errorString) = case e of
        ConfigParseException parseException ->
          ("configuration file parsing", displayException parseException)
        PathException pathException ->
          ("config file path", displayException pathException)

-- | Create a 'LowarnEnv', given the file path of the configuration file being
-- used.
getLowarnEnv :: FilePath -> IO (Either GetEnvException LowarnEnv)
getLowarnEnv configPath =
  runExceptT $ do
    lowarnEnvConfig <-
      catchE
        (ExceptT $ decodeFileEither configPath)
        (throwE . ConfigParseException)
    lowarnEnvConfigPath <-
      catchE (parseSomeFile configPath) (throwE . PathException)
        >>= \case
          Abs absPath -> return absPath
          Rel relPath -> do
            currentDir <- lift getCurrentDir
            return $ currentDir </> relPath
    return $ LowarnEnv {..}
