-- |
-- Module                  : Lowarn.Inject.Preprocessor.GenerateModule
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (GHC)
--
-- Module for the generating @RuntimeDataVar_program_name@ modules.
module Lowarn.Inject.Preprocessor.GenerateModule
  ( RuntimeDataVarModuleInfo (..),
    generateRuntimeDataVarModule,
    parseRuntimeDataVarModuleInfo,
  )
where

import Control.Monad
import Data.Char (isSpace)
import Lowarn.ProgramName (ProgramName, showPrefixModuleName)
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

-- | Type for generating @RuntimeDataVar_program_name@ modules.
data RuntimeDataVarModuleInfo = RuntimeDataVarModuleInfo
  { -- | The name of the program.
    _programName :: ProgramName,
    -- | The module that the state type is imported from, potentially preceded
    -- by the @{-# SOURCE #-}@ pragma.
    _importModule :: String,
    -- | The state type.
    _importType :: String
  }

-- | Generate a @RuntimeDataVar_program_name@ module from
-- 'RuntimeDataVarModuleInfo'.
generateRuntimeDataVarModule :: RuntimeDataVarModuleInfo -> String
generateRuntimeDataVarModule
  RuntimeDataVarModuleInfo
    { _programName = programName,
      _importModule = importModule,
      _importType = importType
    } =
    unlines
      [ printf
          "module %s (runtimeDataVar) where"
          $ showPrefixModuleName "RuntimeDataVar" programName,
        "",
        "import GHC.IO (unsafePerformIO)",
        printf "import %s (%s)" importModule importType,
        "import Lowarn.Inject.RuntimeDataVar (RuntimeDataVar, newRuntimeDataVar)",
        "",
        "{-# NOINLINE runtimeDataVar #-}",
        printf "runtimeDataVar :: RuntimeDataVar %s" importType,
        "runtimeDataVar = unsafePerformIO newRuntimeDataVar"
      ]

-- | A parser for 'RuntimeDataVarModuleInfo', given the 'ProgramName'.
parseRuntimeDataVarModuleInfo :: ProgramName -> ReadP RuntimeDataVarModuleInfo
parseRuntimeDataVarModuleInfo programName = do
  void $ munch isSpace
  void $ string "{-"
  void $ munch isSpace
  void $ string "RUNTIME_DATA_VAR"
  void $ munch1 isSpace
  importModule <- many1 (satisfy $ const True)
  skipMany1 (satisfy isSpace)
  void $ string "("
  importType <- many1 (satisfy $ const True)
  void $ string ")"
  skipMany (satisfy isSpace)
  void $ string "-}"
  void $ munch $ const True
  return $ RuntimeDataVarModuleInfo programName importModule importType
