-- |
-- Module                  : Lowarn.ProgramName
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for program names.
module Lowarn.ProgramName
  ( -- * Type
    ProgramName,
    mkProgramName,
    unProgramName,
    parseProgramName,

    -- * Module names
    showPrefixModuleName,
    parsePrefixModuleName,
  )
where

import Control.Monad
import Lowarn.ParserCombinators
import Text.ParserCombinators.ReadP

-- $setup
-- >>> import Data.Maybe (fromJust)
-- >>> import Lowarn.ParserCombinators (readWithParser)
-- >>> import Text.ParserCombinators.ReadP (readP_to_S)

-- | The name of a program. This is a non-empty sequence of "words" separated by
-- hyphens, where each "word" is a non-empty string of lowercase ASCII letters
-- and digits containing at least one letter.
newtype ProgramName = ProgramName
  { -- | Convert a program name to a 'String'.
    unProgramName :: String
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

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

replaceUnderscoresWithHyphens :: String -> String
replaceUnderscoresWithHyphens = replace '_' '-'

replaceHyphensWithUnderscores :: String -> String
replaceHyphensWithUnderscores = replace '-' '_'

-- | Give the name of a module consisting of a given prefix and the program
-- name. The module name is of the form @Module_foo_bar@, where @Module@ is the
-- prefix and @foo_bar@ is the program name with hyphens replaced with
-- underscores.
--
-- ==== __Examples__
--
-- >>> showPrefixModuleName "EntryPoint" (fromJust $ mkProgramName "foo-bar")
-- "EntryPoint_foo_bar"
showPrefixModuleName :: String -> ProgramName -> String
showPrefixModuleName prefix =
  ((prefix <> "_") <>)
    . replaceHyphensWithUnderscores
    . unProgramName

-- | A parser for program names given a module name that consists of a given
-- prefix and the program name. The module name must be of the form
-- @Module_foo_bar@, where @Module@ is the prefix and @foo_bar@ is the program
-- name with hyphens replaced with underscores.
--
-- ==== __Examples__
--
-- >>> readP_to_S (parsePrefixModuleName "EntryPoint") "EntryPoint_foo_bar"
-- [(ProgramName {unProgramName = "foo"},"_bar"),(ProgramName {unProgramName = "foo-bar"},"")]
--
-- >>> readWithParser (parsePrefixModuleName "EntryPoint") "EntryPoint_foo_bar"
-- Just (ProgramName {unProgramName = "foo-bar"})
--
-- >>> readWithParser (parsePrefixModuleName "EntryPoint") "EntryPoint_foo__bar"
-- Nothing
--
-- >>> readWithParser (parsePrefixModuleName "EntryPoint") "EntryPoint_foo_bar_"
-- Nothing
--
-- >>> readWithParser (parsePrefixModuleName "EntryPoint") "EntryPoint_foo-bar"
-- Nothing
--
-- >>> readWithParser (parsePrefixModuleName "EntryPoint") "EntryPoint_Foo_bar"
-- Nothing
--
-- >>> readWithParser (parsePrefixModuleName "EntryPoint") "EntryPoint_foo1_1bar"
-- Just (ProgramName {unProgramName = "foo1-1bar"})
--
-- >>> readWithParser (parsePrefixModuleName "EntryPoint") "EntryPoint_foo_1"
-- Nothing
--
-- >>> readWithParser (parsePrefixModuleName "EntryPoint") "EntryPoint_fóo_bar"
-- Nothing
--
-- >>> readWithParser (parsePrefixModuleName "EntryPoint") "EntryPoint_"
-- Nothing
--
-- >>> readWithParser (parsePrefixModuleName "EntryPoint") "EntryPoint"
-- Nothing
parsePrefixModuleName :: String -> ReadP ProgramName
parsePrefixModuleName prefix = do
  void $ string prefix
  void $ char '_'
  ProgramName . replaceUnderscoresWithHyphens <$> parseProgramModuleName
