-- |
-- Module                  : Lowarn.TransformerId
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for transformer IDs.
module Lowarn.TransformerId
  ( -- * Type
    TransformerId (..),

    -- * Representation
    showTransformerId,
    parseTransformerId,

    -- * Package names
    showTransformerPackageName,
    parseTransformerPackageName,

    -- * Version IDs
    previousVersionId,
    nextVersionId,
  )
where

import Control.Monad (void)
import Lowarn.ProgramName (ProgramName, parseProgramName, unProgramName)
import Lowarn.VersionId (VersionId (VersionId))
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

-- | A program name and two version numbers, representing a state transformer
-- that transforms state from one version of a program to another, where each
-- version of the program is identified by a version ID consisting of the
-- program name and version's respective version number.
data TransformerId = TransformerId
  { _programName :: ProgramName,
    _previousVersionNumber :: VersionNumber,
    _nextVersionNumber :: VersionNumber
  }
  deriving (Eq, Show)

showWithShows :: String -> (VersionNumber -> String) -> TransformerId -> String
showWithShows
  prefix
  showVersionNumber
  (TransformerId programName previousVersionNumber nextVersionNumber) =
    prefix
      <> unProgramName programName
      <> "-"
      <> showVersionNumber previousVersionNumber
      <> "-"
      <> showVersionNumber nextVersionNumber

-- | Give the transformer ID as a 'String' in the form @foo-bar-1.2.3-1.2.4@,
-- where @foo-bar@ is the program name, @1.2.3@ corresponds to the previous
-- version number, and @1.2.4@ corresponds to the next version number.
--
-- ==== __Examples__
--
-- >>> showTransformerId (TransformerId (fromJust (mkProgramName "foo-bar")) (fromJust (mkVersionNumber (1 :| [2, 3]))) (fromJust (mkVersionNumber (1 :| [2, 4]))))
-- "foo-bar-1.2.3-1.2.4"
showTransformerId :: TransformerId -> String
showTransformerId = showWithShows "" showWithDots

-- | Give the name of the package that contains the transformer ID's
-- corresponding state transformer. The package name is of the form
-- @lowarn-transformer-foo-bar-v1v2v3-v1v2v4@, where @foo-bar@ is the program
-- name, @v1v2v3@ corresponds to the previous version number, and @v1v2v4@
-- corresponds to the next version number.
--
-- ==== __Examples__
--
-- >>> showTransformerPackageName (TransformerId (fromJust (mkProgramName "foo-bar")) (fromJust (mkVersionNumber (1 :| [2, 3]))) (fromJust (mkVersionNumber (1 :| [2, 4]))))
-- "lowarn-transformer-foo-bar-v1v2v3-v1v2v4"
showTransformerPackageName :: TransformerId -> String
showTransformerPackageName =
  showWithShows "lowarn-transformer-" showWithLetters

parseWithParsers :: ReadP () -> ReadP VersionNumber -> ReadP TransformerId
parseWithParsers parsePrefix parseVersionNumber = do
  parsePrefix
  programName <- parseProgramName
  void $ char '-'
  previousVersionNumber <- parseVersionNumber
  void $ char '-'
  TransformerId programName previousVersionNumber <$> parseVersionNumber

-- | A parser for transformer IDs in the form @foo-bar-1.2.3-1.2.4@, where
-- @foo-bar@ is the program name, @1.2.3@ corresponds to the previous version
-- number, and @1.2.4@ corresponds to the next version number.
--
-- ==== __Examples__
--
-- >>> readP_to_S parseTransformerId "foo-bar-1.2.3-1.2.4"
-- [(TransformerId {_programName = ProgramName {unProgramName = "foo-bar"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| []}},".2.4"),(TransformerId {_programName = ProgramName {unProgramName = "foo-bar"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2]}},".4"),(TransformerId {_programName = ProgramName {unProgramName = "foo-bar"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}},"")]
--
-- >>> readWithParser parseTransformerId "foo-bar-1.2.3-1.2.4"
-- Just (TransformerId {_programName = ProgramName {unProgramName = "foo-bar"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}})
--
-- >>> readWithParser parseTransformerId "foo-bar-1.2.3-"
-- Nothing
--
-- >>> readWithParser parseTransformerId "foo-bar-1.2.3"
-- Nothing
--
-- >>> readWithParser parseTransformerId "foo-bar-"
-- Nothing
--
-- >>> readWithParser parseTransformerId "foo-bar"
-- Nothing
--
-- >>> readWithParser parseTransformerId ""
-- Nothing
--
-- >>> readWithParser parseTransformerId "foo-bar-1.2.3-1.2.4-1.2.5"
-- Nothing
parseTransformerId :: ReadP TransformerId
parseTransformerId = parseWithParsers (return ()) parseWithDots

-- | A parser for transformer IDs given the name of the package that contains
-- the transformer ID's corresponding state transformer. The package name must
-- be of the form @lowarn-transformer-foo-bar-v1v2v3-v1v2v4@, where @foo-bar@ is
-- the program name, @v1v2v3@ corresponds to the previous version number, and
-- @v1v2v4@ corresponds to the next version number.
--
-- ==== __Examples__
--
-- >>> readP_to_S parseTransformerPackageName "lowarn-transformer-foo-bar-v1v2v3-v1v2v4"
-- [(TransformerId {_programName = ProgramName {unProgramName = "foo-bar"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| []}},"v2v4"),(TransformerId {_programName = ProgramName {unProgramName = "foo-bar"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2]}},"v4"),(TransformerId {_programName = ProgramName {unProgramName = "foo-bar"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}},"")]
--
-- >>> readWithParser parseTransformerPackageName "lowarn-transformer-foo-bar-v1v2v3-v1v2v4"
-- Just (TransformerId {_programName = ProgramName {unProgramName = "foo-bar"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}})
--
-- >>> readWithParser parseTransformerPackageName "lowarn-transformer-foo-bar-v1v2v3-"
-- Nothing
--
-- >>> readWithParser parseTransformerPackageName "lowarn-transformer-foo-bar-v1v2v3"
-- Nothing
--
-- >>> readWithParser parseTransformerPackageName "lowarn-transformer-foo-bar-"
-- Nothing
--
-- >>> readWithParser parseTransformerPackageName "lowarn-transformer-foo-bar"
-- Nothing
--
-- >>> readWithParser parseTransformerPackageName "lowarn-transformer-"
-- Nothing
--
-- >>> readWithParser parseTransformerPackageName "lowarn-transformer"
-- Nothing
--
-- >>> readWithParser parseTransformerPackageName ""
-- Nothing
--
-- >>> readP_to_S parseTransformerPackageName "lowarn-transformer-foo-bar-v1v2v3-v1v2v4-v1v2v5"
-- [(TransformerId {_programName = ProgramName {unProgramName = "foo-bar"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| []}},"v2v4-v1v2v5"),(TransformerId {_programName = ProgramName {unProgramName = "foo-bar"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2]}},"v4-v1v2v5"),(TransformerId {_programName = ProgramName {unProgramName = "foo-bar"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}},"-v1v2v5"),(TransformerId {_programName = ProgramName {unProgramName = "foo-bar-v1v2v3"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| []}},"v2v5"),(TransformerId {_programName = ProgramName {unProgramName = "foo-bar-v1v2v3"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2]}},"v5"),(TransformerId {_programName = ProgramName {unProgramName = "foo-bar-v1v2v3"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,5]}},"")]
--
-- >>> readWithParser parseTransformerPackageName "lowarn-transformer-foo-bar-v1v2v3-v1v2v4-v1v2v5"
-- Just (TransformerId {_programName = ProgramName {unProgramName = "foo-bar-v1v2v3"}, _previousVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}, _nextVersionNumber = VersionNumber {unVersionNumber = 1 :| [2,5]}})
parseTransformerPackageName :: ReadP TransformerId
parseTransformerPackageName =
  parseWithParsers (void $ string "lowarn-transformer-") parseWithLetters

-- | Give the version ID corresponding to the program that the state transformer
-- corresponding to a given transformer ID transforms state from.
--
-- ==== __Examples__
--
-- >>> previousVersionId (TransformerId (fromJust (mkProgramName "foo-bar")) (fromJust (mkVersionNumber (1 :| [2, 3]))) (fromJust (mkVersionNumber (1 :| [2, 4]))))
-- VersionId {_programName = ProgramName {unProgramName = "foo-bar"}, _versionNumber = VersionNumber {unVersionNumber = 1 :| [2,3]}}
previousVersionId :: TransformerId -> VersionId
previousVersionId (TransformerId programName previousVersionNumber _) =
  VersionId programName previousVersionNumber

-- | Give the version ID corresponding to the program that takes state that has
-- been transformed by the state transformer corresponding to a given
-- transformer ID.
--
-- ==== __Examples__
--
-- >>> nextVersionId (TransformerId (fromJust (mkProgramName "foo-bar")) (fromJust (mkVersionNumber (1 :| [2, 3]))) (fromJust (mkVersionNumber (1 :| [2, 4]))))
-- VersionId {_programName = ProgramName {unProgramName = "foo-bar"}, _versionNumber = VersionNumber {unVersionNumber = 1 :| [2,4]}}
nextVersionId :: TransformerId -> VersionId
nextVersionId (TransformerId programName _ nextVersionNumber) =
  VersionId programName nextVersionNumber
