module Lowarn.ProgramName
  ( ProgramName (..),
    ofEntryPointModuleName,
    ofTransformerModuleName,
    toEntryPointModuleName,
    toTransformerModuleName,
  )
where

import Data.Maybe (listToMaybe)
import Text.Regex.TDFA

newtype ProgramName = ProgramName
  { unProgramName :: String
  }
  deriving (Eq, Show, Read)

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

replaceUnderscoresWithHyphens :: String -> String
replaceUnderscoresWithHyphens = replace '_' '-'

replaceHyphensWithUnderscores :: String -> String
replaceHyphensWithUnderscores = replace '-' '_'

ofModuleName :: String -> String -> Maybe String
ofModuleName prefix moduleName =
  replaceUnderscoresWithHyphens <$> listToMaybe submatches
  where
    (_, _, _, submatches) =
      moduleName =~ ("\\`" <> prefix <> "_(.+)\\'") ::
        (String, String, String, [String])

ofEntryPointModuleName :: String -> Maybe String
ofEntryPointModuleName = ofModuleName "EntryPoint"

ofTransformerModuleName :: String -> Maybe String
ofTransformerModuleName = ofModuleName "Transformer"

toModuleName :: String -> ProgramName -> String
toModuleName prefix =
  ((prefix <> "_") <>)
    . replaceHyphensWithUnderscores
    . unProgramName

toEntryPointModuleName :: ProgramName -> String
toEntryPointModuleName = toModuleName "EntryPoint"

toTransformerModuleName :: ProgramName -> String
toTransformerModuleName = toModuleName "Transformer"
