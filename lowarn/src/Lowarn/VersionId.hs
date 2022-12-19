module Lowarn.VersionId
  ( VersionId (..),
    showVersionId,
    showVersionPackageName,
    parseVersionId,
    parseVersionPackageName,
  )
where

import Control.Monad (void)
import Lowarn.ParserCombinators (parsePackageName)
import Lowarn.ProgramName (ProgramName (..))
import Lowarn.VersionNumber
  ( VersionNumber,
    parseWithDots,
    parseWithLetters,
    showWithDots,
    showWithLetters,
  )
import Text.ParserCombinators.ReadP

data VersionId = VersionId
  { _programName :: ProgramName,
    _versionNumber :: VersionNumber
  }
  deriving (Eq, Show, Read)

showVersionId :: VersionId -> String
showVersionId (VersionId programName versionNumber) =
  unProgramName programName <> "-" <> showWithDots versionNumber

showVersionPackageName :: VersionId -> String
showVersionPackageName (VersionId programName versionNumber) =
  "lowarn-version-"
    <> unProgramName programName
    <> "-"
    <> showWithLetters versionNumber

parseWithParsers :: ReadP () -> ReadP VersionNumber -> ReadP VersionId
parseWithParsers parsePrefix parseVersionNumber = do
  parsePrefix
  programName <- ProgramName <$> parsePackageName
  void $ char '-'
  VersionId programName <$> parseVersionNumber

parseVersionId :: ReadP VersionId
parseVersionId = parseWithParsers (return ()) parseWithDots

parseVersionPackageName :: ReadP VersionId
parseVersionPackageName =
  parseWithParsers (void $ string "lowarn-version-") parseWithLetters
