-- |
-- Module                  : Lowarn.ParserCombinators
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for helper functions for using parser combinators.
module Lowarn.ParserCombinators
  ( -- * Using parsers
    readWithParser,

    -- * Parsers
    parsePackageName,
    parseProgramModuleName,
  )
where

import Control.Applicative (liftA2)
import Data.Char (isAsciiLower, isDigit)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP

-- $setup
-- >>> import Text.ParserCombinators.ReadP (many, readP_to_S, satisfy, string, (+++))

-- | Read a value from a 'String' using a 'ReadP' parser. The parser must accept
-- the entire input, without any trailing whitespace. If the parse is ambiguous,
-- the first result is returned.
--
-- ==== __Examples__
--
-- >>> readWithParser (string "foo") "foo"
-- Just "foo"
--
-- >>> readWithParser (string "foo") "foo "
-- Nothing
--
-- >>> readWithParser (string "foo") " foo"
-- Nothing
--
-- >>> readWithParser (many (satisfy $ const True)) "foo"
-- Just "foo"
--
-- >>> readWithParser (string "foo" *> (return 1 +++ return 2)) "foo"
-- Just 1
readWithParser :: ReadP a -> String -> Maybe a
readWithParser parser = fmap fst . listToMaybe . readP_to_S (parser <* eof)

parsePackageWord :: ReadP String
parsePackageWord =
  concat
    <$> sequence
      [ munch isDigit,
        munch1 isAsciiLower,
        munch (liftA2 (||) isAsciiLower isDigit)
      ]

parsePackageWordSequence :: Char -> ReadP String
parsePackageWordSequence separator =
  intercalate [separator] <$> sepBy1 parsePackageWord (char separator)

-- | A parser for package names, which are non-empty sequences of "words"
-- separated by hyphens, where each "word" is a non-empty string of lowercase
-- ASCII letters and digits containing at least one letter.
--
-- ==== __Examples__
--
-- >>> readP_to_S parsePackageName "foo-bar"
-- [("foo","-bar"),("foo-bar","")]
--
-- >>> readWithParser parsePackageName "foo-bar"
-- Just "foo-bar"
--
-- >>> readWithParser parsePackageName "foo--bar"
-- Nothing
--
-- >>> readWithParser parsePackageName "foo-bar-"
-- Nothing
--
-- >>> readWithParser parsePackageName "foo_bar"
-- Nothing
--
-- >>> readWithParser parsePackageName "foo bar"
-- Nothing
--
-- >>> readWithParser parsePackageName "Foo-bar"
-- Nothing
--
-- >>> readWithParser parsePackageName "foo1-1bar"
-- Just "foo1-1bar"
--
-- >>> readWithParser parsePackageName "foo-1"
-- Nothing
--
-- >>> readWithParser parsePackageName "fóo_bar"
-- Nothing
--
-- >>> readWithParser parsePackageName ""
-- Nothing
parsePackageName :: ReadP String
parsePackageName = parsePackageWordSequence '-'

-- | A parser for program module names, which are non-empty sequences of "words"
-- separated by underscores, where each "word" is a non-empty string of
-- lowercase ASCII letters and digits containing at least one letter.
--
-- ==== __Examples__
--
-- >>> readP_to_S parseProgramModuleName "foo_bar"
-- [("foo","_bar"),("foo_bar","")]
--
-- >>> readWithParser parseProgramModuleName "foo_bar"
-- Just "foo_bar"
--
-- >>> readWithParser parseProgramModuleName "foo__bar"
-- Nothing
--
-- >>> readWithParser parseProgramModuleName "foo_bar_"
-- Nothing
--
-- >>> readWithParser parseProgramModuleName "foo-bar"
-- Nothing
--
-- >>> readWithParser parseProgramModuleName "foo bar"
-- Nothing
--
-- >>> readWithParser parseProgramModuleName "Foo_bar"
-- Nothing
--
-- >>> readWithParser parseProgramModuleName "foo1_1bar"
-- Just "foo1_1bar"
--
-- >>> readWithParser parseProgramModuleName "foo_1"
-- Nothing
--
-- >>> readWithParser parseProgramModuleName "fóo_bar"
-- Nothing
--
-- >>> readWithParser parseProgramModuleName ""
-- Nothing
parseProgramModuleName :: ReadP String
parseProgramModuleName = parsePackageWordSequence '_'
