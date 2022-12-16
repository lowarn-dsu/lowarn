{-# LANGUAGE LambdaCase #-}

module Lowarn.ProgramVersion
  ( ProgramVersion (..),
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

newtype ProgramVersion = ProgramVersion
  { unProgramVersion :: NonEmpty Int
  }
  deriving (Eq, Ord, Show, Read)

showWithSeparator :: String -> ProgramVersion -> String
showWithSeparator separator = intercalate separator . map show . toList . unProgramVersion

showWithDots :: ProgramVersion -> String
showWithDots = showWithSeparator "."

showWithLetters :: ProgramVersion -> String
showWithLetters = ("v" <>) . showWithSeparator "v"

parseWithDots :: ReadP ProgramVersion
parseWithDots =
  sepBy1 (read <$> munch1 isDigit) (char '.') >>= \case
    [] -> pfail
    (x : xs) -> return $ ProgramVersion (x :| xs)

parseWithLetters :: ReadP ProgramVersion
parseWithLetters =
  char 'v' *> sepBy1 (read <$> munch1 isDigit) (char 'v') >>= \case
    [] -> pfail
    (x : xs) -> return $ ProgramVersion (x :| xs)
