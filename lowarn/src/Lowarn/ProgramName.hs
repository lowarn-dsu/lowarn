module Lowarn.ProgramName
  ( ProgramName (..),
    ofVersionModuleName,
    ofTransformerModuleName,
    toVersionModuleName,
    toTransformerModuleName,
  )
where

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
ofModuleName prefix moduleName = case submatches of
  [programName] -> Just $ replaceUnderscoresWithHyphens programName
  _ -> Nothing
  where
    (_, _, _, submatches) =
      moduleName =~ ("\\`" <> prefix <> "_(.+)\\'") ::
        (String, String, String, [String])

ofVersionModuleName :: String -> Maybe String
ofVersionModuleName = ofModuleName "Version"

ofTransformerModuleName :: String -> Maybe String
ofTransformerModuleName = ofModuleName "Transformer"

toModuleName :: String -> ProgramName -> String
toModuleName prefix =
  ((prefix <> "_") <>)
    . replaceHyphensWithUnderscores
    . unProgramName

toVersionModuleName :: ProgramName -> String
toVersionModuleName = toModuleName "Version"

toTransformerModuleName :: ProgramName -> String
toTransformerModuleName = toModuleName "Transformer"
