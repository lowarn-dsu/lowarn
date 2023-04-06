-- |
-- Module                  : Lowarn.VersionNumber
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for version numbers.
module Lowarn.VersionNumber
  ( -- * Type
    VersionNumber,
    mkVersionNumber,
    unVersionNumber,

    -- * Representations
    showWithDots,
    showWithLetters,
    parseWithDots,
    parseWithLetters,

    -- * Exports
    showEntryPointExport,
    showUpdateExport,
  )
where

import Data.Char
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Text.ParserCombinators.ReadP

-- $setup
-- >>> import Data.Maybe (fromJust, listToMaybe)
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> import Lowarn.ParserCombinators (readWithParser)
-- >>> import Text.ParserCombinators.ReadP (readP_to_S)

-- | The version number of a program. This is a non-empty sequence of
-- non-negative integers.
--
-- Version numbers are compared lexicographically, so @1.2@ is less than
-- @1.2.0@.
newtype VersionNumber = VersionNumber
  { unVersionNumber :: NonEmpty Int
  }
  deriving (Eq, Ord, Show)

-- | Create a 'VersionNumber' from a 'NonEmpty' list of 'Int's. If any number is
-- negative, 'Nothing' is returned.
--
-- ==== __Examples__
--
-- >>> mkVersionNumber (1 :| [2, 3])
-- Just (VersionNumber {unVersionNumber = 1 :| [2,3]})
--
-- >>> mkVersionNumber (1 :| [-2, 3])
-- Nothing
--
-- >>> mkVersionNumber (1 :| [])
-- Just (VersionNumber {unVersionNumber = 1 :| []})
--
-- >>> mkVersionNumber (0 :| [])
-- Just (VersionNumber {unVersionNumber = 0 :| []})
mkVersionNumber :: NonEmpty Int -> Maybe VersionNumber
mkVersionNumber versionNumberComponents =
  if all (>= 0) versionNumberComponents
    then Just $ VersionNumber versionNumberComponents
    else Nothing

showWithSeparator :: String -> VersionNumber -> String
showWithSeparator separator =
  intercalate separator . map show . toList . unVersionNumber

-- | Show a 'VersionNumber' as a string of dot-separated integers.
--
-- ==== __Examples__
--
-- >>> showWithDots (fromJust $ mkVersionNumber (1 :| [2, 3]))
-- "1.2.3"
showWithDots :: VersionNumber -> String
showWithDots = showWithSeparator "."

-- | Show a 'VersionNumber' as a string of integers that are each preceded by
-- the letter "v".
--
-- ==== __Examples__
--
-- >>> showWithLetters (fromJust $ mkVersionNumber (1 :| [2, 3]))
-- "v1v2v3"
showWithLetters :: VersionNumber -> String
showWithLetters = ("v" <>) . showWithSeparator "v"

-- | Show the identifier that is used to export the entry point of a program's
-- version that corresponds to a given version number.
--
-- ==== __Examples__
--
-- >>> showEntryPointExport (fromJust $ mkVersionNumber (1 :| [2, 3]))
-- "hs_entryPoint_v1v2v3"
showEntryPointExport :: VersionNumber -> String
showEntryPointExport = ("hs_entryPoint_" <>) . showWithLetters

-- | Show the identifier that is used to export an update from one version of a
-- program to another, given the version's corresponding version numbers.
--
-- ==== __Examples__
--
-- >>> showUpdateExport (fromJust $ mkVersionNumber (1 :| [2, 3])) (fromJust $ mkVersionNumber (1 :| [2, 4]))
-- "hs_update_v1v2v3_v1v2v4"
showUpdateExport :: VersionNumber -> VersionNumber -> String
showUpdateExport previousVersionNumber nextVersionNUmber =
  "hs_update_"
    <> showWithLetters previousVersionNumber
    <> "_"
    <> showWithLetters nextVersionNUmber

parseWithSeparator :: Char -> ReadP VersionNumber
parseWithSeparator separator = do
  xs <- sepBy1 (read <$> string "0" <++ munch1 isDigit) (char separator)
  case xs of
    (y : ys) -> return $ VersionNumber (y :| ys)
    [] -> pfail

-- | A parser for version numbers given as strings of dot-separated non-negative
-- integers.
--
-- ==== __Examples__
--
-- >>> readP_to_S parseWithDots "1.2.3"
-- [(VersionNumber {unVersionNumber = 1 :| []},".2.3"),(VersionNumber {unVersionNumber = 1 :| [2]},".3"),(VersionNumber {unVersionNumber = 1 :| [2,3]},"")]
--
-- >>> readWithParser parseWithDots "1.2.3"
-- Just (VersionNumber {unVersionNumber = 1 :| [2,3]})
--
-- >>> readWithParser parseWithDots "1"
-- Just (VersionNumber {unVersionNumber = 1 :| []})
--
-- >>> readWithParser parseWithDots "1.2..3"
-- Nothing
--
-- >>> readWithParser parseWithDots "1.2.3."
-- Nothing
--
-- >>> readWithParser parseWithDots "1.-2.3"
-- Nothing
--
-- >>> readWithParser parseWithDots "10.20.30"
-- Just (VersionNumber {unVersionNumber = 10 :| [20,30]})
--
-- >>> readWithParser parseWithDots "0"
-- Just (VersionNumber {unVersionNumber = 0 :| []})
--
-- >>> readWithParser parseWithDots "01.002.0003"
-- Nothing
--
-- >>> readWithParser parseWithDots ""
-- Nothing
parseWithDots :: ReadP VersionNumber
parseWithDots = parseWithSeparator '.'

-- | A parser for version numbers given as strings of non-negative integers that
-- are each preceded by the letter "v".
--
-- ==== __Examples__
--
-- >>> readP_to_S parseWithLetters "v1v2v3"
-- [(VersionNumber {unVersionNumber = 1 :| []},"v2v3"),(VersionNumber {unVersionNumber = 1 :| [2]},"v3"),(VersionNumber {unVersionNumber = 1 :| [2,3]},"")]
--
-- >>> readWithParser parseWithLetters "v1v2v3"
-- Just (VersionNumber {unVersionNumber = 1 :| [2,3]})
--
-- >>> readWithParser parseWithLetters "v1"
-- Just (VersionNumber {unVersionNumber = 1 :| []})
--
-- >>> readWithParser parseWithLetters "v1v2vv3"
-- Nothing
--
-- >>> readWithParser parseWithLetters "v1v2v3v"
-- Nothing
--
-- >>> readWithParser parseWithLetters "v1v-v2v3"
-- Nothing
--
-- >>> readWithParser parseWithLetters "v10v20v30"
-- Just (VersionNumber {unVersionNumber = 10 :| [20,30]})
--
-- >>> readWithParser parseWithLetters "v0"
-- Just (VersionNumber {unVersionNumber = 0 :| []})
--
-- >>> readWithParser parseWithLetters "v01v002v0003"
-- Nothing
--
-- >>> readWithParser parseWithLetters ""
-- Nothing
parseWithLetters :: ReadP VersionNumber
parseWithLetters = char 'v' *> parseWithSeparator 'v'
