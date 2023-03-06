{-# LANGUAGE InstanceSigs #-}
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
import Control.Monad.Trans.Except
import Data.Yaml
import Lowarn.Cli.Config
import Path
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
newtype GetEnvException
  = ConfigParseException ParseException
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

-- | Create a 'LowarnEnv', given the file path of the configuration file being
-- used.
getLowarnEnv :: Path Abs File -> IO (Either GetEnvException LowarnEnv)
getLowarnEnv lowarnEnvConfigPath =
  runExceptT $ do
    lowarnEnvConfig <-
      catchE
        (ExceptT $ decodeFileEither $ toFilePath lowarnEnvConfigPath)
        (throwE . ConfigParseException)
    return $ LowarnEnv {..}
