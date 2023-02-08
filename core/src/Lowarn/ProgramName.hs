-- |
-- Module                  : Lowarn.ProgramName
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for program names.
module Lowarn.ProgramName
  ( -- * Type
    ProgramName,
    mkProgramName,
    unProgramName,
    parseProgramName,

    -- * Module names
    showEntryPointModuleName,
    showUpdateModuleName,
    parseEntryPointModuleName,
    parseUpdateModuleName,
  )
where

import Control.Monad (void)
import Lowarn.ParserCombinators
  ( parsePackageName,
    parseProgramModuleName,
    readWithParser,
  )
import Text.ParserCombinators.ReadP

-- $setup
-- >>> import Data.Maybe (fromJust)
-- >>> import Lowarn.ParserCombinators (readWithParser)
-- >>> import Text.ParserCombinators.ReadP (readP_to_S)

-- | The name of a program. This is a non-empty sequence of "words" separated by
-- hyphens, where each "word" is a non-empty string of lowercase ASCII letters
-- and digits containing at least one letter.
newtype ProgramName = ProgramName
  { unProgramName :: String
  }
  deriving (Eq, Show)

-- | Create a 'ProgramName' from a 'String'.
--
-- ==== __Examples__
--
-- >>> mkProgramName "foo"
-- Just (ProgramName {unProgramName = "foo"})
--
-- >>> mkProgramName "foo-bar"
-- Just (ProgramName {unProgramName = "foo-bar"})
--
-- >>> mkProgramName "foo--bar"
-- Nothing
--
-- >>> mkProgramName "foo-bar-"
-- Nothing
--
-- >>> mkProgramName "foo_bar"
-- Nothing
--
-- >>> mkProgramName "foo bar"
-- Nothing
--
-- >>> mkProgramName "Foo-bar"
-- Nothing
--
-- >>> mkProgramName "foo1-1bar"
-- Just (ProgramName {unProgramName = "foo1-1bar"})
--
-- >>> mkProgramName "foo-1"
-- Nothing
--
-- >>> mkProgramName "fóo_bar"
-- Nothing
--
-- >>> mkProgramName ""
-- Nothing
mkProgramName :: String -> Maybe ProgramName
mkProgramName = readWithParser parseProgramName

-- | A parser for program names.
parseProgramName :: ReadP ProgramName
parseProgramName = ProgramName <$> parsePackageName

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

-- | Give the name of the module that contains an entry point for the program
-- with the given name. The module name is of the form @EntryPoint_foo_bar@,
-- where @foo_bar@ is the program name with hyphens replaced with underscores.
--
-- ==== __Examples__
--
-- >>> showEntryPointModuleName (fromJust $ mkProgramName "foo-bar")
-- "EntryPoint_foo_bar"
showEntryPointModuleName :: ProgramName -> String
showEntryPointModuleName = showModuleNameWithPrefix "EntryPoint"

-- | Give the name of the module that contains an update for the program with
-- the given name. The module name is of the form @Update_foo_bar@, where
-- @foo_bar@ is the program name with hyphens replaced with underscores.
--
-- ==== __Examples__
--
-- >>> showUpdateModuleName (fromJust $ mkProgramName "foo-bar")
-- "Update_foo_bar"
showUpdateModuleName :: ProgramName -> String
showUpdateModuleName = showModuleNameWithPrefix "Update"

parseModuleNameWithPrefix :: String -> ReadP ProgramName
parseModuleNameWithPrefix prefix = do
  void $ string prefix
  void $ char '_'
  ProgramName . replaceUnderscoresWithHyphens <$> parseProgramModuleName

-- | A parser for program names given a corresponding entry point module name.
-- The module name must be of the form @EntryPoint_foo_bar@, where @foo_bar@ is
-- the program name with hyphens replaced with underscores.
--
-- ==== __Examples__
--
-- >>> readP_to_S parseEntryPointModuleName "EntryPoint_foo_bar"
-- [(ProgramName {unProgramName = "foo"},"_bar"),(ProgramName {unProgramName = "foo-bar"},"")]
--
-- >>> readWithParser parseEntryPointModuleName "EntryPoint_foo_bar"
-- Just (ProgramName {unProgramName = "foo-bar"})
--
-- >>> readWithParser parseEntryPointModuleName "EntryPoint_foo__bar"
-- Nothing
--
-- >>> readWithParser parseEntryPointModuleName "EntryPoint_foo_bar_"
-- Nothing
--
-- >>> readWithParser parseEntryPointModuleName "EntryPoint_foo-bar"
-- Nothing
--
-- >>> readWithParser parseEntryPointModuleName "EntryPoint_Foo_bar"
-- Nothing
--
-- >>> readWithParser parseEntryPointModuleName "EntryPoint_foo1_1bar"
-- Just (ProgramName {unProgramName = "foo1-1bar"})
--
-- >>> readWithParser parseEntryPointModuleName "EntryPoint_foo_1"
-- Nothing
--
-- >>> readWithParser parseEntryPointModuleName "EntryPoint_fóo_bar"
-- Nothing
--
-- >>> readWithParser parseEntryPointModuleName "EntryPoint_"
-- Nothing
--
-- >>> readWithParser parseEntryPointModuleName "EntryPoint"
-- Nothing
parseEntryPointModuleName :: ReadP ProgramName
parseEntryPointModuleName = parseModuleNameWithPrefix "EntryPoint"

-- | A parser for program names given a corresponding update module name. The
-- module name must be of the form @Update_foo_bar@, where @foo_bar@ is the
-- program name with hyphens replaced with underscores.
--
-- ==== __Examples__
--
-- >>> readP_to_S parseUpdateModuleName "Update_foo_bar"
-- [(ProgramName {unProgramName = "foo"},"_bar"),(ProgramName {unProgramName = "foo-bar"},"")]
--
-- >>> readWithParser parseUpdateModuleName "Update_foo_bar"
-- Just (ProgramName {unProgramName = "foo-bar"})
--
-- >>> readWithParser parseUpdateModuleName "Update_foo__bar"
-- Nothing
--
-- >>> readWithParser parseUpdateModuleName "Update_foo_bar_"
-- Nothing
--
-- >>> readWithParser parseUpdateModuleName "Update_foo-bar"
-- Nothing
--
-- >>> readWithParser parseUpdateModuleName "Update_Foo_bar"
-- Nothing
--
-- >>> readWithParser parseUpdateModuleName "Update_foo1_1bar"
-- Just (ProgramName {unProgramName = "foo1-1bar"})
--
-- >>> readWithParser parseUpdateModuleName "Update_foo_1"
-- Nothing
--
-- >>> readWithParser parseUpdateModuleName "Update_fóo_bar"
-- Nothing
--
-- >>> readWithParser parseUpdateModuleName "Update_"
-- Nothing
--
-- >>> readWithParser parseUpdateModuleName "Update"
-- Nothing
--
-- >>> readWithParser parseUpdateModuleName ""
-- Nothing
parseUpdateModuleName :: ReadP ProgramName
parseUpdateModuleName = parseModuleNameWithPrefix "Update"
