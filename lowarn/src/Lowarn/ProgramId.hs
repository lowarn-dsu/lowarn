module Lowarn.ProgramId
  ( ProgramId (..),
    showProgramId,
    parseProgramId,
    ofVersionPackageName,
    ofTransformerPackageName,
    toVersionPackageName,
    toTransformerPackageName,
  )
where

import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Lowarn.ProgramName (ProgramName (..))
import Lowarn.ProgramVersion
  ( ProgramVersion,
    parseWithDots,
    parseWithLetters,
    showWithDots,
    showWithLetters,
  )
import Text.ParserCombinators.ReadP
import Text.Regex.TDFA

data ProgramId = ProgramId
  { _programName :: ProgramName,
    _programVersion :: ProgramVersion
  }
  deriving (Eq, Show, Read)

showProgramId :: ProgramId -> String
showProgramId (ProgramId programName programVersion) =
  unProgramName programName <> "-" <> showWithDots programVersion

parseProgramId :: ReadP ProgramId
parseProgramId = do
  name <- many1 (satisfy $ const True)
  void $ char '-'
  ProgramId (ProgramName name) <$> parseWithDots

ofPackageName :: String -> String -> Maybe ProgramId
ofPackageName prefix packageName = case submatches of
  programName : programVersion : _ ->
    ProgramId
      (ProgramName programName)
      <$> (fst <$> listToMaybe (readP_to_S parseWithLetters programVersion))
  _ -> Nothing
  where
    (_, _, _, submatches) =
      packageName =~ ("\\`lowarn-" <> prefix <> "-(.+)-((v[[:digit:]]+)+)\\'") ::
        (String, String, String, [String])

ofVersionPackageName :: String -> Maybe ProgramId
ofVersionPackageName = ofPackageName "version"

ofTransformerPackageName :: String -> Maybe ProgramId
ofTransformerPackageName = ofPackageName "transformer"

toPackageName :: String -> ProgramId -> String
toPackageName prefix (ProgramId programName programVersion) =
  "lowarn-"
    <> prefix
    <> "-"
    <> unProgramName programName
    <> "-"
    <> showWithLetters programVersion

toVersionPackageName :: ProgramId -> String
toVersionPackageName = toPackageName "version"

toTransformerPackageName :: ProgramId -> String
toTransformerPackageName = toPackageName "transformer"
