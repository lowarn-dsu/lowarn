{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module                  : Lowarn.Inject.Preprocessor
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (GHC)
--
-- Module for the Lowarn injection pre-processor.
module Lowarn.Inject.Preprocessor (processFile, parseModuleName) where

import Control.Monad
import Data.Char (isSpace)
import Data.List (intercalate, isSuffixOf)
import Lowarn.Inject.Preprocessor.GenerateModule
import Lowarn.ParserCombinators (readWithParser)
import Lowarn.ProgramName
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

-- $setup
-- >>> import Data.Maybe (fromJust)
-- >>> import Lowarn.ProgramName (mkProgramName)
-- >>> import Text.ParserCombinators.ReadP (readP_to_S)

-- | Pre-process the source of a Haskell module to support runtime data
-- injection.
--
-- The resulting code will reference the original code. For @.hs-boot@ files,
-- nothing more will happen. For @RuntimeDataVar_program_name@ modules, where
-- @program-name@ is the given program name, the output will be Haskell code
-- that exports a @'Lowarn.Inject.RuntimeDataVar' t@, where @t@ is a type
-- specified in the module. For other modules, the injection plugin will be
-- enabled and the @RuntimeDataVar_program_name@ module will be referenced such
-- that modules are compiled in the correct order.
--
-- ==== __Examples__
--
-- >>> putStr $ processFile "path/file.hs" (fromJust (mkProgramName "foo-bar")) "module RuntimeDataVar_foo_bar () where\n\n{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n"
-- {-# LINE 1 "path/file.hs" #-}
-- module RuntimeDataVar_foo_bar (runtimeDataVar) where
-- <BLANKLINE>
-- import GHC.IO (unsafePerformIO)
-- import Lowarn.Inject.RuntimeDataVar (RuntimeDataVar, newRuntimeDataVar)
-- import {-# SOURCE #-} Lowarn.ExampleProgram.Following (State)
-- <BLANKLINE>
-- {-# NOINLINE runtimeDataVar #-}
-- runtimeDataVar :: RuntimeDataVar State
-- runtimeDataVar = unsafePerformIO newRuntimeDataVar
--
-- >>> putStr $ processFile "path/file.hs" (fromJust (mkProgramName "foo-bar")) "module NotRuntimeDataVar_foo_bar () where\n\n{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n"
-- {-# LINE 1 "path/file.hs" #-}
-- {-# OPTIONS_GHC -fplugin=Lowarn.Inject.Plugin #-}
-- module NotRuntimeDataVar_foo_bar () where
-- <BLANKLINE>
-- <BLANKLINE>
-- import RuntimeDataVar_foo_bar ()
-- {- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}
--
-- >>> putStr $ processFile "path/file.hs" (fromJust (mkProgramName "foo-bar")) "module RuntimeDataVar_other_program () where\n\n{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n"
-- {-# LINE 1 "path/file.hs" #-}
-- {-# OPTIONS_GHC -fplugin=Lowarn.Inject.Plugin #-}
-- module RuntimeDataVar_other_program () where
-- <BLANKLINE>
-- <BLANKLINE>
-- import RuntimeDataVar_foo_bar ()
-- {- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}
--
-- >>> putStr $ processFile "path/file.hs-boot" (fromJust (mkProgramName "foo-bar")) "module RuntimeDataVar_foo_bar () where\n\n{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n"
-- {-# LINE 1 "path/file.hs-boot" #-}
-- module RuntimeDataVar_foo_bar () where
-- <BLANKLINE>
-- {- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}
--
-- >>> putStr $ processFile "path/file.hs-boot" (fromJust (mkProgramName "foo-bar")) "module NotRuntimeDataVar_foo_bar () where\n\n{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n"
-- {-# LINE 1 "path/file.hs-boot" #-}
-- module NotRuntimeDataVar_foo_bar () where
-- <BLANKLINE>
-- {- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}
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
              | parsedProgramName == programName ->
                  maybe
                    inputModule
                    generateRuntimeDataVarModule
                    $ readWithParser
                      (parseRuntimeDataVarModuleInfo programName)
                      after
            _ ->
              intercalate
                "\n"
                [ "{-# OPTIONS_GHC -fplugin=Lowarn.Inject.Plugin #-}",
                  printf
                    "{-# OPTIONS_GHC -fplugin-opt=Lowarn.Inject.Plugin:%s #-}"
                    $ unProgramName programName,
                  before,
                  printf
                    "import %s ()"
                    (showPrefixModuleName "RuntimeDataVar" programName),
                  after
                ]
        _ -> inputModule

-- | Parse up to the module header of the source of a Haskell module and return
-- the parsed name of the module.
--
-- ==== __Examples__
--
-- >>> readP_to_S parseModuleName "module RuntimeDataVar_following () where\n\n{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n"
-- [("RuntimeDataVar_following","{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n")]
--
-- >>> readP_to_S parseModuleName "{-# LANGUAGE DataKinds #-}\n\nmodule RuntimeDataVar_following () where\n\n{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n"
-- [("RuntimeDataVar_following","{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n")]
--
-- >>> readP_to_S parseModuleName "module RuntimeDataVar_following where\n\n{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n"
-- [("RuntimeDataVar_following","{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n")]
--
-- >>> readP_to_S parseModuleName "module RuntimeDataVar_following (State) where\n\n{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n"
-- [("RuntimeDataVar_following","{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n")]
--
-- >>> readP_to_S parseModuleName "module   RuntimeDataVar_following   (   )   where\n\n{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n"
-- [("RuntimeDataVar_following","{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n")]
--
-- >>> readP_to_S parseModuleName "module \n RuntimeDataVar_following \n ( \n ) \n where\n\n{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n"
-- [("RuntimeDataVar_following","{- RUNTIME_DATA_VAR {-# SOURCE #-} Lowarn.ExampleProgram.Following (State) -}\n")]
--
-- >>> readP_to_S parseModuleName "module RuntimeDataVar_following () where"
-- [("RuntimeDataVar_following","")]
parseModuleName :: ReadP String
parseModuleName = do
  optional $ do
    skipMany (satisfy $ const True)
    satisfy isSpace
  void $ string "module"
  skipMany1 (satisfy isSpace)
  moduleName <- munch1 (not . isSpace)
  skipMany1 (satisfy isSpace)
  optional $ do
    void $ char '('
    skipMany (satisfy $ const True)
    void $ char ')'
    skipMany1 (satisfy isSpace)
  void $ string "where"
  void $ munch isSpace
  return moduleName
