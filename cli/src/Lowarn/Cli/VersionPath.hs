{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.Cli.VersionPath
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for converting between version numbers and paths.
module Lowarn.Cli.VersionPath
  ( pathToVersionNumber,
    versionNumberToPath,
  )
where

import Data.Maybe
import Lowarn.Cli.Env
import Lowarn.ParserCombinators (readWithParser)
import Lowarn.VersionNumber
import Path
import System.FilePath (dropTrailingPathSeparator)

getBaseDirectory :: Path Rel Dir -> Path Rel Dir
getBaseDirectory path =
  if dirname path == path
    then path
    else getBaseDirectory (parent path)

-- | Convert a path to a version number if the path is
-- @config-path/versions/1.2.3@, where @config-path@ is the path of the
-- directory containing the Lowarn configuration file and @1.2.3@ is the version
-- number.
pathToVersionNumber :: LowarnEnv -> Path Abs Dir -> Maybe VersionNumber
pathToVersionNumber LowarnEnv {..} path = do
  versionPath <-
    stripProperPrefix
      (parent lowarnEnvConfigPath </> [reldir|versions|])
      path
  readWithParser parseWithDots $
    dropTrailingPathSeparator $
      toFilePath $
        getBaseDirectory versionPath

-- | Convert a version number to a path @config-path/versions/1.2.3@, where
-- @config-path@ is the path of the directory containing the Lowarn
-- configuration file and @1.2.3@ is the version number.
versionNumberToPath :: LowarnEnv -> VersionNumber -> Path Abs Dir
versionNumberToPath LowarnEnv {..} versionNumber =
  parent lowarnEnvConfigPath
    </> [reldir|versions|]
    </> fromJust (parseRelDir $ showWithDots versionNumber)
