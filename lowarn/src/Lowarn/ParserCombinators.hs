module Lowarn.ParserCombinators
  ( parsePackageName,
    parseProgramModuleName,
    runParser,
  )
where

import Control.Applicative (liftA2)
import Data.Char (isAsciiLower, isDigit)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP

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
  intercalate [separator] <$> sepBy parsePackageWord (char separator)

parsePackageName :: ReadP String
parsePackageName = parsePackageWordSequence '-'

parseProgramModuleName :: ReadP String
parseProgramModuleName = parsePackageWordSequence '_'

runParser :: ReadP a -> String -> Maybe a
runParser parser = fmap fst . listToMaybe . readP_to_S (parser <* eof)
