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
import Lowarn.ParserCombinators (readWithParser)
import Lowarn.VersionNumber
import Path
import System.FilePath (dropTrailingPathSeparator)

-- $setup
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> import Data.Maybe (fromJust)
-- >>> import Lowarn.VersionNumber (mkVersionNumber)
-- >>> import Path (absdir)

getBaseDirectory :: Path Rel Dir -> Path Rel Dir
getBaseDirectory path =
  if dirname path == path
    then path
    else getBaseDirectory (parent path)

-- | Convert a path to a version number if the path is
-- @config-path/versions/1.2.3@, where @config-path@ is the path of the
-- directory containing a Lowarn CLI program and @1.2.3@ is the version number.
--
-- ==== __Examples__
--
-- >>> pathToVersionNumber [absdir|/foo/|] [absdir|/foo/versions/1.2.3/|]
-- Just (VersionNumber {unVersionNumber = 1 :| [2,3]})
--
-- >>> pathToVersionNumber [absdir|/foo/|] [absdir|/foo/versions/1.2.3/bar/baz/|]
-- Just (VersionNumber {unVersionNumber = 1 :| [2,3]})
--
-- >>> pathToVersionNumber [absdir|/foo/|] [absdir|/foo/versions/v1v2v3/|]
-- Nothing
--
-- >>> pathToVersionNumber [absdir|/foo/|] [absdir|/foo/1.2.3/|]
-- Nothing
--
-- >>> pathToVersionNumber [absdir|/foo/|] [absdir|/bar/versions/1.2.3/|]
-- Nothing
--
-- >>> pathToVersionNumber [absdir|/foo/|] [absdir|/foo/versions/1.2.3/versions/1.2.4/|]
-- Just (VersionNumber {unVersionNumber = 1 :| [2,3]})
--
-- >>> pathToVersionNumber [absdir|/versions/1.2.3|] [absdir|/versions/1.2.3/foo/|]
-- Nothing
pathToVersionNumber :: Path Abs Dir -> Path Abs Dir -> Maybe VersionNumber
pathToVersionNumber programPath path = do
  versionPath <-
    stripProperPrefix
      (programPath </> [reldir|versions|])
      path
  readWithParser parseWithDots $
    dropTrailingPathSeparator $
      toFilePath $
        getBaseDirectory versionPath

-- | Convert a version number to a path @config-path/versions/1.2.3@, where
-- @config-path@ is the path of the directory containing the Lowarn
-- configuration file and @1.2.3@ is the version number.
--
-- ==== __Examples__
--
-- >>> versionNumberToPath [absdir|/foo/|] (fromJust $ mkVersionNumber (1 :| [2, 3]))
-- "/foo/versions/1.2.3/"
versionNumberToPath :: Path Abs Dir -> VersionNumber -> Path Abs Dir
versionNumberToPath programPath versionNumber =
  programPath
    </> [reldir|versions|]
    </> fromJust (parseRelDir $ showWithDots versionNumber)
