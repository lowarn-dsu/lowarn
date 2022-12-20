-- |
-- Module                  : Lowarn.VersionId
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for version IDs.
module Lowarn.VersionId
  ( -- * Type
    VersionId (..),

    -- * Representation
    showVersionId,
    parseVersionId,

    -- * Package names
    showVersionPackageName,
    parseVersionPackageName,
  )
where

import Control.Monad (void)
import Lowarn.ProgramName (ProgramName, parseProgramName, unProgramName)
import Lowarn.VersionNumber
  ( VersionNumber,
    parseWithDots,
    parseWithLetters,
    showWithDots,
    showWithLetters,
  )
import Text.ParserCombinators.ReadP

-- $setup
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> import Data.Maybe (fromJust)
-- >>> import Lowarn.ParserCombinators (readWithParser)
-- >>> import Lowarn.ProgramName (mkProgramName)
-- >>> import Lowarn.VersionNumber (mkVersionNumber)
-- >>> import Text.ParserCombinators.ReadP (readP_to_S)

-- | A program name and a version number, representing a version of a program.
data VersionId = VersionId
  { _programName :: ProgramName,
    _versionNumber :: VersionNumber
  }
  deriving (Eq, Show)

-- | Give the version ID as a 'String' in the form @foo-bar-1.2.3@, where
-- @foo-bar@ is the program name and @1.2.3@ corresponds to the version number.
--
-- ==== __Examples__
--
-- >>> showVersionId (VersionId (fromJust (mkProgramName "foo-bar")) (fromJust (mkVersionNumber (1 :| [2, 3]))))
-- "foo-bar-1.2.3"
showVersionId :: VersionId -> String
showVersionId (VersionId programName versionNumber) =
  unProgramName programName <> "-" <> showWithDots versionNumber

-- | Give the name of the package that contains the version ID's corresponding
-- version, including its entry point. The package name is of the form
-- @lowarn-version-foo-bar-v1v2v3@, where @foo-bar@ is the program name and
-- @1.2.3@ corresponds to the version number.
--
-- ==== __Examples__
--
-- >>> showVersionPackageName (VersionId (fromJust (mkProgramName "foo-bar")) (fromJust (mkVersionNumber (1 :| [2, 3]))))
-- "lowarn-version-foo-bar-v1v2v3"
showVersionPackageName :: VersionId -> String
showVersionPackageName (VersionId programName versionNumber) =
  "lowarn-version-"
    <> unProgramName programName
    <> "-"
    <> showWithLetters versionNumber

parseWithParsers :: ReadP () -> ReadP VersionNumber -> ReadP VersionId
parseWithParsers parsePrefix parseVersionNumber = do
  parsePrefix
  programName <- parseProgramName
  void $ char '-'
  VersionId programName <$> parseVersionNumber

-- | A parser for version IDs in the form @foo-bar-1.2.3@, where @foo-bar@ is
-- the program name and @1.2.3@ corresponds to the version number.
--
-- ==== __Examples__
--
-- >>> readP_to_S parseVersionId "foo-bar-1.2.3"
-- [(VersionId {_programName = ProgramName {unProgramName = "foo-bar"}, _versionNumber = VersionNumber {unVersionNumber = 1 :| []}},".2.3"),(VersionId {_programName = ProgramName {unProgramName = "foo-bar"}, _versionNumber = VersionNumber {unVersionNumber = 1 :| [2]}},".3"),(VersionId {_programName = ProgramName {unProgramName = "foo-bar"}, _versionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}},"")]
--
-- >>> readWithParser parseVersionId "foo-bar-1.2.3"
-- Just (VersionId {_programName = ProgramName {unProgramName = "foo-bar"}, _versionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}})
parseVersionId :: ReadP VersionId
parseVersionId = parseWithParsers (return ()) parseWithDots

-- | A parser for version IDs given the name of the package that contains the
-- version ID's corresponding version, including its entry point. The package
-- name must be of the form @lowarn-version-foo-bar-v1v2v3@, where @foo-bar@ is
-- the program name and @v1v2v3@ corresponds to the version number.
--
-- ==== __Examples__
--
-- >>> readP_to_S parseVersionPackageName "lowarn-version-foo-bar-v1v2v3"
-- [(VersionId {_programName = ProgramName {unProgramName = "foo-bar"}, _versionNumber = VersionNumber {unVersionNumber = 1 :| []}},"v2v3"),(VersionId {_programName = ProgramName {unProgramName = "foo-bar"}, _versionNumber = VersionNumber {unVersionNumber = 1 :| [2]}},"v3"),(VersionId {_programName = ProgramName {unProgramName = "foo-bar"}, _versionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}},"")]
--
-- >>> readWithParser parseVersionPackageName "lowarn-version-foo-bar-v1v2v3"
-- Just (VersionId {_programName = ProgramName {unProgramName = "foo-bar"}, _versionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}})
parseVersionPackageName :: ReadP VersionId
parseVersionPackageName =
  parseWithParsers (void $ string "lowarn-version-") parseWithLetters
