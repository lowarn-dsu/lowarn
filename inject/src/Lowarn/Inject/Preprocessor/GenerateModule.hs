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

-- $setup
-- >>> import Data.Maybe (fromJust)
-- >>> import Lowarn.ParserCombinators (readWithParser)
-- >>> import Lowarn.ProgramName (mkProgramName)
-- >>> import Text.ParserCombinators.ReadP (readP_to_S)

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
  deriving (Show)

-- | Generate a @RuntimeDataVar_program_name@ module from
-- 'RuntimeDataVarModuleInfo'.
--
-- ==== __Examples__
--
-- >>> putStrLn $ generateRuntimeDataVarModule (RuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar")) "Lowarn.ExampleProgram.Following" "State")
-- module RuntimeDataVar_foo_bar (runtimeDataVar) where
-- <BLANKLINE>
-- import GHC.IO (unsafePerformIO)
-- import Lowarn.Inject.RuntimeDataVar (RuntimeDataVar, newRuntimeDataVar)
-- import Lowarn.ExampleProgram.Following (State)
-- <BLANKLINE>
-- {-# NOINLINE runtimeDataVar #-}
-- runtimeDataVar :: RuntimeDataVar State
-- runtimeDataVar = unsafePerformIO newRuntimeDataVar
-- <BLANKLINE>
--
-- >>> putStrLn $ generateRuntimeDataVarModule (RuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar")) "{-# SOURCE #-} Lowarn.ExampleProgram.Following" "State")
-- module RuntimeDataVar_foo_bar (runtimeDataVar) where
-- <BLANKLINE>
-- import GHC.IO (unsafePerformIO)
-- import Lowarn.Inject.RuntimeDataVar (RuntimeDataVar, newRuntimeDataVar)
-- import {-# SOURCE #-} Lowarn.ExampleProgram.Following (State)
-- <BLANKLINE>
-- {-# NOINLINE runtimeDataVar #-}
-- runtimeDataVar :: RuntimeDataVar State
-- runtimeDataVar = unsafePerformIO newRuntimeDataVar
-- <BLANKLINE>
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
        "import Lowarn.Inject.RuntimeDataVar (RuntimeDataVar, newRuntimeDataVar)",
        printf "import %s (%s)" importModule importType,
        "",
        "{-# NOINLINE runtimeDataVar #-}",
        printf "runtimeDataVar :: RuntimeDataVar %s" importType,
        "runtimeDataVar = unsafePerformIO newRuntimeDataVar"
      ]

-- | A parser for 'RuntimeDataVarModuleInfo', given the 'ProgramName'.
--
-- ==== __Examples__
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}"
-- [(RuntimeDataVarModuleInfo {_programName = ProgramName {unProgramName = "foo-bar"}, _importModule = "{-# SOURCE #-} Lowarn.ExampleProgram.Following", _importType = "State"},"")]
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "   {-   RUNTIME_DATA_VAR   {-# SOURCE #-} Lowarn.ExampleProgram.Following   (State)   -}   "
-- [(RuntimeDataVarModuleInfo {_programName = ProgramName {unProgramName = "foo-bar"}, _importModule = "{-# SOURCE #-} Lowarn.ExampleProgram.Following", _importType = "State"},"")]
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) " \n {-   RUNTIME_DATA_VAR \n {-# SOURCE #-} Lowarn.ExampleProgram.Following \n (State) \n -} \n "
-- [(RuntimeDataVarModuleInfo {_programName = ProgramName {unProgramName = "foo-bar"}, _importModule = "{-# SOURCE #-} Lowarn.ExampleProgram.Following", _importType = "State"},"")]
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- RUNTIME_DATA_VAR Lowarn.ExampleProgram.Following (State) -}"
-- [(RuntimeDataVarModuleInfo {_programName = ProgramName {unProgramName = "foo-bar"}, _importModule = "Lowarn.ExampleProgram.Following", _importType = "State"},"")]
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- RUNTIME_DATA_VAR (State) -}"
-- []
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- Lowarn.ExampleProgram.Following (State) -}"
-- []
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State1, State2) -}"
-- [(RuntimeDataVarModuleInfo {_programName = ProgramName {unProgramName = "foo-bar"}, _importModule = "{-# SOURCE #-} Lowarn.ExampleProgram.Following", _importType = "State1, State2"},"")]
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following () -}"
-- []
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following -}"
-- []
parseRuntimeDataVarModuleInfo :: ProgramName -> ReadP RuntimeDataVarModuleInfo
parseRuntimeDataVarModuleInfo programName = do
  void $ munch isSpace
  void $ string "{-"
  void $ munch isSpace
  void $ string "RUNTIME_DATA_VAR"
  void $ munch1 isSpace
  importModule <-
    liftM2
      (:)
      (satisfy $ const True)
      ( manyTill (satisfy $ const True) $ do
          skipMany1 (satisfy isSpace)
          void $ string "("
      )
  importType <- many1 (satisfy $ const True)
  void $ string ")"
  skipMany (satisfy isSpace)
  void $ string "-}"
  void $ munch $ const True
  return $ RuntimeDataVarModuleInfo programName importModule importType
