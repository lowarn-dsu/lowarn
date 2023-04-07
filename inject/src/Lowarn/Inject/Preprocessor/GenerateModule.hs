{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.Inject.Preprocessor.GenerateModule
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for the generating @RuntimeDataVar_program_name@ modules.
module Lowarn.Inject.Preprocessor.GenerateModule
  ( RuntimeDataVarModuleInfo (..),
    generateRuntimeDataVarModule,
    parseRuntimeDataVarModuleInfo,
  )
where

import Control.Monad
import Data.Char
import Lowarn.ParserCombinators
import Lowarn.ProgramName
import Text.ParserCombinators.ReadP
import Text.Printf

-- $setup
-- >>> import Data.Maybe (fromJust)
-- >>> import Lowarn.ParserCombinators (readWithParser)
-- >>> import Lowarn.ProgramName (mkProgramName)
-- >>> import Text.ParserCombinators.ReadP (readP_to_S)

-- | Type for generating @RuntimeDataVar_program_name@ modules.
data RuntimeDataVarModuleInfo = RuntimeDataVarModuleInfo
  { -- | The name of the program.
    runtimeDataVarModuleInfoProgramName :: ProgramName,
    -- | The module that the state type is imported from, potentially preceded
    -- by the @{-# SOURCE #-}@ pragma.
    runtimeDataVarModuleInfoImportModule :: String,
    -- | The state type.
    runtimeDataVarModuleInfoImportType :: String
  }
  deriving (Show)

-- | Generate a @RuntimeDataVar_program_name@ module from
-- 'RuntimeDataVarModuleInfo'.
--
-- ==== __Examples__
--
-- >>> putStr $ generateRuntimeDataVarModule (RuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar")) "Lowarn.ExampleProgram.Following" "State")
-- module RuntimeDataVar_foo_bar (runtimeDataVar) where
-- <BLANKLINE>
-- import GHC.IO (unsafePerformIO)
-- import Lowarn.Inject.RuntimeDataVar (RuntimeDataVar, newRuntimeDataVar)
-- import Lowarn.ExampleProgram.Following (State)
-- <BLANKLINE>
-- {-# NOINLINE runtimeDataVar #-}
-- runtimeDataVar :: RuntimeDataVar State
-- runtimeDataVar = unsafePerformIO newRuntimeDataVar
--
-- >>> putStr $ generateRuntimeDataVarModule (RuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar")) "{-# SOURCE #-} Lowarn.ExampleProgram.Following" "State")
-- module RuntimeDataVar_foo_bar (runtimeDataVar) where
-- <BLANKLINE>
-- import GHC.IO (unsafePerformIO)
-- import Lowarn.Inject.RuntimeDataVar (RuntimeDataVar, newRuntimeDataVar)
-- import {-# SOURCE #-} Lowarn.ExampleProgram.Following (State)
-- <BLANKLINE>
-- {-# NOINLINE runtimeDataVar #-}
-- runtimeDataVar :: RuntimeDataVar State
-- runtimeDataVar = unsafePerformIO newRuntimeDataVar
generateRuntimeDataVarModule :: RuntimeDataVarModuleInfo -> String
generateRuntimeDataVarModule RuntimeDataVarModuleInfo {..} =
  unlines
    [ printf
        "module %s (runtimeDataVar) where"
        $ showPrefixModuleName
          "RuntimeDataVar"
          runtimeDataVarModuleInfoProgramName,
      "",
      "import GHC.IO (unsafePerformIO)",
      "import Lowarn.Inject.RuntimeDataVar (RuntimeDataVar, newRuntimeDataVar)",
      printf
        "import %s (%s)"
        runtimeDataVarModuleInfoImportModule
        runtimeDataVarModuleInfoImportType,
      "",
      "{-# NOINLINE runtimeDataVar #-}",
      printf
        "runtimeDataVar :: RuntimeDataVar %s"
        runtimeDataVarModuleInfoImportType,
      "runtimeDataVar = unsafePerformIO newRuntimeDataVar"
    ]

-- | A parser for 'RuntimeDataVarModuleInfo', given the 'ProgramName'.
--
-- ==== __Examples__
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}"
-- [(RuntimeDataVarModuleInfo {runtimeDataVarModuleInfoProgramName = ProgramName {unProgramName = "foo-bar"}, runtimeDataVarModuleInfoImportModule = "{-# SOURCE #-} Lowarn.ExampleProgram.Following", runtimeDataVarModuleInfoImportType = "State"},"")]
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "   {-   RUNTIME_DATA_VAR   {-# SOURCE #-} Lowarn.ExampleProgram.Following   (State)   -}   "
-- [(RuntimeDataVarModuleInfo {runtimeDataVarModuleInfoProgramName = ProgramName {unProgramName = "foo-bar"}, runtimeDataVarModuleInfoImportModule = "{-# SOURCE #-} Lowarn.ExampleProgram.Following", runtimeDataVarModuleInfoImportType = "State"},"")]
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) " \n {-   RUNTIME_DATA_VAR \n {-# SOURCE #-} Lowarn.ExampleProgram.Following \n (State) \n -} \n "
-- [(RuntimeDataVarModuleInfo {runtimeDataVarModuleInfoProgramName = ProgramName {unProgramName = "foo-bar"}, runtimeDataVarModuleInfoImportModule = "{-# SOURCE #-} Lowarn.ExampleProgram.Following", runtimeDataVarModuleInfoImportType = "State"},"")]
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- RUNTIME_DATA_VAR Lowarn.ExampleProgram.Following (State) -}"
-- [(RuntimeDataVarModuleInfo {runtimeDataVarModuleInfoProgramName = ProgramName {unProgramName = "foo-bar"}, runtimeDataVarModuleInfoImportModule = "Lowarn.ExampleProgram.Following", runtimeDataVarModuleInfoImportType = "State"},"")]
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- RUNTIME_DATA_VAR (State) -}"
-- []
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- Lowarn.ExampleProgram.Following (State) -}"
-- []
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State1, State2) -}"
-- [(RuntimeDataVarModuleInfo {runtimeDataVarModuleInfoProgramName = ProgramName {unProgramName = "foo-bar"}, runtimeDataVarModuleInfoImportModule = "{-# SOURCE #-} Lowarn.ExampleProgram.Following", runtimeDataVarModuleInfoImportType = "State1, State2"},"")]
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following () -}"
-- []
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following -}"
-- []
--
-- >>> readP_to_S (parseRuntimeDataVarModuleInfo (fromJust (mkProgramName "foo-bar"))) "{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n\n{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}"
-- [(RuntimeDataVarModuleInfo {runtimeDataVarModuleInfoProgramName = ProgramName {unProgramName = "foo-bar"}, runtimeDataVarModuleInfoImportModule = "{-# SOURCE #-} Lowarn.ExampleProgram.Following", runtimeDataVarModuleInfoImportType = "State"},"")]
parseRuntimeDataVarModuleInfo :: ProgramName -> ReadP RuntimeDataVarModuleInfo
parseRuntimeDataVarModuleInfo runtimeDataVarModuleInfoProgramName = do
  void $ munch isSpace
  void $ string "{-"
  void $ munch isSpace
  void $ string "RUNTIME_DATA_VAR"
  void $ munch1 isSpace
  runtimeDataVarModuleInfoImportModule <-
    manyTill1 (satisfy $ const True) $ do
      skipMany1 (satisfy isSpace)
      void $ char '('
  runtimeDataVarModuleInfoImportType <-
    manyTill1 (satisfy $ const True) (char ')')
  skipMany (satisfy isSpace)
  void $ string "-}"
  void $ munch $ const True
  return RuntimeDataVarModuleInfo {..}
