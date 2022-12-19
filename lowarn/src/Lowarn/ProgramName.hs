module Lowarn.ProgramName
  ( ProgramName (..),
    showEntryPointModuleName,
    showTransformerModuleName,
    parseEntryPointModuleName,
    parseTransformerModuleName,
  )
where

import Control.Monad (void)
import Lowarn.ParserCombinators (parseProgramModuleName)
import Text.ParserCombinators.ReadP

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

showModuleNameWithPrefix :: String -> ProgramName -> String
showModuleNameWithPrefix prefix =
  ((prefix <> "_") <>)
    . replaceHyphensWithUnderscores
    . unProgramName

showEntryPointModuleName :: ProgramName -> String
showEntryPointModuleName = showModuleNameWithPrefix "EntryPoint"

showTransformerModuleName :: ProgramName -> String
showTransformerModuleName = showModuleNameWithPrefix "Transformer"

parseModuleNameWithPrefix :: String -> ReadP String
parseModuleNameWithPrefix prefix = do
  void $ string prefix
  void $ char '_'
  replaceUnderscoresWithHyphens <$> parseProgramModuleName

parseEntryPointModuleName :: ReadP String
parseEntryPointModuleName = parseModuleNameWithPrefix "EntryPoint"

parseTransformerModuleName :: ReadP String
parseTransformerModuleName = parseModuleNameWithPrefix "Transformer"
