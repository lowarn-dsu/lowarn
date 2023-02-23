{-# LANGUAGE ScopedTypeVariables #-}

module Lowarn.Inject.Preprocessor (processFile) where

import Control.Monad
import Data.Char (isSpace)
import Data.List (isSuffixOf)
import Lowarn.ParserCombinators (readWithParser)
import Lowarn.ProgramName
  ( ProgramName,
    parsePrefixModuleName,
    showPrefixModuleName,
  )
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

processFile :: FilePath -> ProgramName -> String -> String
processFile originalPath programName inputModule =
  printf "{-# LINE 1 \"%s\" #-}\n" originalPath
    <> if ".hs-boot" `isSuffixOf` originalPath
      then inputModule
      else case readP_to_S (gather parseModuleName) inputModule of
        ((before, moduleName), after) : _ ->
          case readWithParser
            (parsePrefixModuleName "RuntimeDataVar")
            moduleName of
            Just parsedProgramName
              | parsedProgramName == programName -> inputModule
            _ ->
              before
                <> printf
                  "\nimport %s ()\n"
                  (showPrefixModuleName "RuntimeDataVar" programName)
                <> after
        _ -> inputModule

parseModuleName :: ReadP String
parseModuleName = do
  optional $ do
    skipMany (satisfy $ const True)
    satisfy isSpace
  void $ string "module"
  skipMany1 (satisfy isSpace)
  moduleName <- munch1 (not . isSpace)
  skipMany1 (satisfy isSpace)
  skipMany (satisfy $ const True)
  void $ satisfy isSpace
  void $ string "where"
  skipMany1 (satisfy isSpace)
  return moduleName