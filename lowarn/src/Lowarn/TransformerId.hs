module Lowarn.TransformerId
  ( TransformerId (..),
    showTransformerId,
    showTransformerPackageName,
    parseTransformerId,
    parseTransformerPackageName,
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

data TransformerId = TransformerId
  { _programName :: ProgramName,
    _previousVersionNumber :: VersionNumber,
    _nextVersionNumber :: VersionNumber
  }
  deriving (Eq, Show, Read)

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

showTransformerId :: TransformerId -> String
showTransformerId = showWithShows "" showWithDots

showTransformerPackageName :: TransformerId -> String
showTransformerPackageName =
  showWithShows "lowarn-transformer-" showWithLetters

parseWithParsers :: ReadP () -> ReadP VersionNumber -> ReadP TransformerId
parseWithParsers parsePrefix parseVersionNumber = do
  parsePrefix
  programName <- ProgramName <$> parsePackageName
  void $ char '-'
  previousVersionNumber <- parseVersionNumber
  void $ char '-'
  TransformerId programName previousVersionNumber <$> parseVersionNumber

parseTransformerId :: ReadP TransformerId
parseTransformerId = parseWithParsers (return ()) parseWithDots

parseTransformerPackageName :: ReadP TransformerId
parseTransformerPackageName =
  parseWithParsers (void $ string "lowarn-transformer-") parseWithLetters
