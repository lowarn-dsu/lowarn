{-# LANGUAGE TupleSections #-}

-- |
-- Module                  : Lowarn.Cli.VersionGraph
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (GHC)
--
-- Module for Lowarn program version graphs.
module Lowarn.Cli.VersionGraph (VersionGraph (..), getVersionGraph) where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lowarn.ParserCombinators
import Lowarn.ProgramName
import Lowarn.UpdateId
import Lowarn.VersionId
import Lowarn.VersionNumber
import Path
import Path.IO
import Text.ParserCombinators.ReadP

-- | Type for directed graphs with vertices for versions of a program and edges
-- for updates between these versions.
newtype VersionGraph = VersionGraph
  { unVersionGraph :: Map VersionNumber (Set VersionNumber)
  }

getPackages ::
  ReadP a ->
  (ProgramName -> a -> b) ->
  (b -> String) ->
  Path Abs Dir ->
  ProgramName ->
  IO [a]
getPackages
  parseVersionNumbers
  mkId
  showPackageName
  projectDirectory
  programName = do
    (subdirectories, _) <- listDir projectDirectory
    map fst
      <$> filterM
        snd
        [ ( versionNumbers,
            doesPathExist $ versionDirectory </> versionCabalPath
          )
          | versionDirectory <- subdirectories,
            Just versionNumbers <-
              return $
                readWithParser parseVersionNumbers $
                  toFilePath $
                    dirname versionDirectory,
            versionCabalPath <-
              parseRelFile $
                showPackageName (mkId programName versionNumbers) <> ".cabal"
        ]

getVersions :: Path Abs Dir -> ProgramName -> IO [VersionNumber]
getVersions = getPackages parseWithDots VersionId showVersionPackageName

getUpdates :: Path Abs Dir -> ProgramName -> IO [(VersionNumber, VersionNumber)]
getUpdates =
  getPackages
    (liftA2 (,) parseWithDots (char '-' >> parseWithDots))
    (uncurry . UpdateId)
    showUpdatePackageName

-- | Get a 'VersionGraph' by looking at the structure of a given directory. Each
-- version should be in a directory @versions/1.2.3@, where @1.2.3@ represents a
-- version number, that has a file @lowarn-version-program-name-v1v2v3@, where
-- @program-name@ is the given program name. Each update should be in a
-- directory @updates/1.2.3-1.2.3@, where @1.2.3@ and @1.2.4@ represent previous
-- and next version numbers, that has a file
-- @lowarn-update-program-name-v1v2v3-v1v2v4@, where @program-name@ is the given
-- program name.
getVersionGraph :: Path Abs Dir -> ProgramName -> IO VersionGraph
getVersionGraph searchDir programName = do
  versions <- getVersions searchDir programName
  updates <- getUpdates searchDir programName
  return $
    VersionGraph $
      foldl'
        ( \versionMap (fromVersion, toVersion) ->
            Map.adjust (Set.insert fromVersion) toVersion versionMap
        )
        (Map.fromList $ map (,Set.empty) versions)
        updates
