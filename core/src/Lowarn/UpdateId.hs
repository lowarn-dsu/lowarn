{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.UpdateId
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for update IDs.
module Lowarn.UpdateId
  ( -- * Type
    UpdateId (..),

    -- * Representation
    showUpdateId,
    parseUpdateId,

    -- * Package names
    showUpdatePackageName,
    parseUpdatePackageName,

    -- * Version IDs
    previousVersionId,
    nextVersionId,
  )
where

import Control.Monad
import Lowarn.ProgramName
import Lowarn.VersionId
import Lowarn.VersionNumber
import Text.ParserCombinators.ReadP

-- $setup
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> import Data.Maybe (fromJust)
-- >>> import Lowarn.ParserCombinators (readWithParser)
-- >>> import Lowarn.ProgramName (mkProgramName)
-- >>> import Lowarn.VersionNumber (mkVersionNumber)
-- >>> import Text.ParserCombinators.ReadP (readP_to_S)

-- | A program name and two version numbers, representing an update from one
-- version of a program to another, where each version of the program is
-- identified by a version ID consisting of the program name and version's
-- respective version number.
data UpdateId = UpdateId
  { updateIdProgramName :: ProgramName,
    updateIdPreviousVersionNumber :: VersionNumber,
    updateIdNextVersionNumber :: VersionNumber
  }
  deriving (Eq, Show)

showWithShows :: String -> (VersionNumber -> String) -> UpdateId -> String
showWithShows prefix showVersionNumber UpdateId {..} =
  prefix
    <> unProgramName updateIdProgramName
    <> "-"
    <> showVersionNumber updateIdPreviousVersionNumber
    <> "-"
    <> showVersionNumber updateIdNextVersionNumber

-- | Give the update ID as a 'String' in the form @foo-bar-1.2.3-1.2.4@,
-- where @foo-bar@ is the program name, @1.2.3@ corresponds to the previous
-- version number, and @1.2.4@ corresponds to the next version number.
--
-- ==== __Examples__
--
-- >>> showUpdateId (UpdateId (fromJust (mkProgramName "foo-bar")) (fromJust (mkVersionNumber (1 :| [2, 3]))) (fromJust (mkVersionNumber (1 :| [2, 4]))))
-- "foo-bar-1.2.3-1.2.4"
showUpdateId :: UpdateId -> String
showUpdateId = showWithShows "" showWithDots

-- | Give the name of the package that contains the update ID's
-- corresponding update. The package name is of the form
-- @lowarn-update-foo-bar-v1v2v3-v1v2v4@, where @foo-bar@ is the program
-- name, @v1v2v3@ corresponds to the previous version number, and @v1v2v4@
-- corresponds to the next version number.
--
-- ==== __Examples__
--
-- >>> showUpdatePackageName (UpdateId (fromJust (mkProgramName "foo-bar")) (fromJust (mkVersionNumber (1 :| [2, 3]))) (fromJust (mkVersionNumber (1 :| [2, 4]))))
-- "lowarn-update-foo-bar-v1v2v3-v1v2v4"
showUpdatePackageName :: UpdateId -> String
showUpdatePackageName = showWithShows "lowarn-update-" showWithLetters

parseWithParsers :: ReadP () -> ReadP VersionNumber -> ReadP UpdateId
parseWithParsers parsePrefix parseVersionNumber = do
  parsePrefix
  updateIdProgramName <- parseProgramName
  void $ char '-'
  updateIdPreviousVersionNumber <- parseVersionNumber
  void $ char '-'
  updateIdNextVersionNumber <- parseVersionNumber
  return UpdateId {..}

-- | A parser for update IDs in the form @foo-bar-1.2.3-1.2.4@, where
-- @foo-bar@ is the program name, @1.2.3@ corresponds to the previous version
-- number, and @1.2.4@ corresponds to the next version number.
--
-- ==== __Examples__
--
-- >>> readP_to_S parseUpdateId "foo-bar-1.2.3-1.2.4"
-- [(UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| []}},".2.4"),(UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2]}},".4"),(UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}},"")]
--
-- >>> readWithParser parseUpdateId "foo-bar-1.2.3-1.2.4"
-- Just (UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}})
--
-- >>> readWithParser parseUpdateId "foo-bar-1.2.3-"
-- Nothing
--
-- >>> readWithParser parseUpdateId "foo-bar-1.2.3"
-- Nothing
--
-- >>> readWithParser parseUpdateId "foo-bar-"
-- Nothing
--
-- >>> readWithParser parseUpdateId "foo-bar"
-- Nothing
--
-- >>> readWithParser parseUpdateId ""
-- Nothing
--
-- >>> readWithParser parseUpdateId "foo-bar-1.2.3-1.2.4-1.2.5"
-- Nothing
parseUpdateId :: ReadP UpdateId
parseUpdateId = parseWithParsers (return ()) parseWithDots

-- | A parser for update IDs given the name of the package that contains
-- the update ID's corresponding update. The package name must be of the form
-- @lowarn-update-foo-bar-v1v2v3-v1v2v4@, where @foo-bar@ is the program name,
-- @v1v2v3@ corresponds to the previous version number, and @v1v2v4@ corresponds
-- to the next version number.
--
-- ==== __Examples__
--
-- >>> readP_to_S parseUpdatePackageName "lowarn-update-foo-bar-v1v2v3-v1v2v4"
-- [(UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| []}},"v2v4"),(UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2]}},"v4"),(UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}},"")]
--
-- >>> readWithParser parseUpdatePackageName "lowarn-update-foo-bar-v1v2v3-v1v2v4"
-- Just (UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}})
--
-- >>> readWithParser parseUpdatePackageName "lowarn-update-foo-bar-v1v2v3-"
-- Nothing
--
-- >>> readWithParser parseUpdatePackageName "lowarn-update-foo-bar-v1v2v3"
-- Nothing
--
-- >>> readWithParser parseUpdatePackageName "lowarn-update-foo-bar-"
-- Nothing
--
-- >>> readWithParser parseUpdatePackageName "lowarn-update-foo-bar"
-- Nothing
--
-- >>> readWithParser parseUpdatePackageName "lowarn-update-"
-- Nothing
--
-- >>> readWithParser parseUpdatePackageName "lowarn-update"
-- Nothing
--
-- >>> readWithParser parseUpdatePackageName ""
-- Nothing
--
-- >>> readP_to_S parseUpdatePackageName "lowarn-update-foo-bar-v1v2v3-v1v2v4-v1v2v5"
-- [(UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| []}},"v2v4-v1v2v5"),(UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2]}},"v4-v1v2v5"),(UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}},"-v1v2v5"),(UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar-v1v2v3"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| []}},"v2v5"),(UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar-v1v2v3"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2]}},"v5"),(UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar-v1v2v3"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,5]}},"")]
--
-- >>> readWithParser parseUpdatePackageName "lowarn-update-foo-bar-v1v2v3-v1v2v4-v1v2v5"
-- Just (UpdateId {updateIdProgramName = ProgramName {unProgramName = "foo-bar-v1v2v3"}, updateIdPreviousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}, updateIdNextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,5]}})
parseUpdatePackageName :: ReadP UpdateId
parseUpdatePackageName =
  parseWithParsers (void $ string "lowarn-update-") parseWithLetters

-- | Give the version ID corresponding to the version of the program that the
-- update corresponding to the update ID updates from.
--
-- ==== __Examples__
--
-- >>> previousVersionId (UpdateId (fromJust (mkProgramName "foo-bar")) (fromJust (mkVersionNumber (1 :| [2, 3]))) (fromJust (mkVersionNumber (1 :| [2, 4]))))
-- VersionId {versionIdProgramName = ProgramName {unProgramName = "foo-bar"}, versionIdVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}}
previousVersionId :: UpdateId -> VersionId
previousVersionId UpdateId {..} =
  VersionId updateIdProgramName updateIdPreviousVersionNumber

-- | Give the version ID corresponding to the version of the program that the
-- update corresponding to the update ID updates to.
--
-- ==== __Examples__
--
-- >>> nextVersionId (UpdateId (fromJust (mkProgramName "foo-bar")) (fromJust (mkVersionNumber (1 :| [2, 3]))) (fromJust (mkVersionNumber (1 :| [2, 4]))))
-- VersionId {versionIdProgramName = ProgramName {unProgramName = "foo-bar"}, versionIdVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}}
nextVersionId :: UpdateId -> VersionId
nextVersionId UpdateId {..} =
  VersionId updateIdProgramName updateIdNextVersionNumber
