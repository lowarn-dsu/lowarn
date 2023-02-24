{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module                  : Lowarn.Inject.Preprocessor
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (GHC)
--
-- Module for the Lowarn injection pre-processor.
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

-- | Pre-process the source of a Haskell module to support runtime data
-- injection.
--
-- The resulting code will reference the original code. For @.hs-boot@ files,
-- nothing more will happen. For @RuntimeDataVar_program-name@ modules, where
-- @program-name@ is a given program name, the output will be Haskell code that
-- exports a @'RuntimeDataVar' t@, where @t@ is a type specified in the module.
-- For other modules, the injection plugin will be enabled and the
-- @RuntimeDataVar_program-name@ module will be referenced such that modules are
-- compiled in the correct order.
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
              "{-# OPTIONS_GHC -fplugin=Lowarn.Inject.Plugin #-}\n"
                <> before
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
