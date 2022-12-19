{-# LANGUAGE LambdaCase #-}

module Lowarn.VersionNumber
  ( VersionNumber (..),
    showWithDots,
    showWithLetters,
    parseWithDots,
    parseWithLetters,
  )
where

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Text.ParserCombinators.ReadP

newtype VersionNumber = VersionNumber
  { unVersionNumber :: NonEmpty Int
  }
  deriving (Eq, Ord, Show, Read)

showWithSeparator :: String -> VersionNumber -> String
showWithSeparator separator =
  intercalate separator . map show . toList . unVersionNumber

showWithDots :: VersionNumber -> String
showWithDots = showWithSeparator "."

showWithLetters :: VersionNumber -> String
showWithLetters = ("v" <>) . showWithSeparator "v"

parseWithSeparator :: Char -> ReadP VersionNumber
parseWithSeparator separator =
  sepBy1 (read <$> munch1 isDigit) (char separator) >>= \case
    (x : xs) -> return $ VersionNumber (x :| xs)
    [] -> pfail

parseWithDots :: ReadP VersionNumber
parseWithDots = parseWithSeparator '.'

parseWithLetters :: ReadP VersionNumber
parseWithLetters = char 'v' *> parseWithSeparator 'v'
