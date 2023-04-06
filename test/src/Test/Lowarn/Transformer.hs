{-# LANGUAGE LambdaCase #-}

-- |
-- Module                  : Test.Lowarn.Transformer
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for testing transformers with Lowarn.
module Test.Lowarn.Transformer
  ( Import (Import),
    Expression (Expression),
    transformerGoldenTest,
  )
where

import Data.Functor
import Data.List (isPrefixOf)
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import Test.Lowarn.Golden
import Test.Tasty
import Text.Printf

-- | Type for modules that should be imported during the test. This is typically
-- used for adding orphan instances.
newtype Import = Import
  { unImport :: String
  }

-- | Type for a Haskell expression to be run.
newtype Expression = Expression
  { unExpression :: String
  }

ghcErrorsToString :: [GhcError] -> String
ghcErrorsToString = unlines . map errMsg

addNewline :: String -> String
addNewline s = if last s == '\n' then s else s <> "\n"

testHaskell :: [Import] -> Expression -> IO String
testHaskell imports ioExpression =
  runInterpreter
    ( do
        unsafeSetGhcOption "-fconstraint-solver-iterations=12"
        setImportsQ $
          [ ("Prelude", Nothing),
            ("Lowarn", Nothing),
            ("Lowarn.Transformer", Nothing),
            ("Generics.SOP.Universe", Just "Generics.SOP.Universe"),
            ("Data.SOP.Constraint", Just "Data.SOP.Constraint")
          ]
            <> map
              (\x -> ("Test.Lowarn.Type.Types" <> x, Just x))
              ["A", "B", "C", "D", "E"]
            <> map
              (\i -> (unImport i, Nothing))
              imports
        typeChecksWithDetails (unExpression ioExpression) >>= \case
          Left ghcErrors ->
            return
              $ printf
                "Type checking errors:\n%s"
              $ ghcErrorsToString
                ghcErrors
          Right ioExpressionType
            | isPrefixOf "IO " ioExpressionType -> do
                runStmt $ printf "___x <- %s" (unExpression ioExpression)
                eval "___x"
            | otherwise ->
                return $
                  printf
                    "Expression type %s does not begin with \"IO \""
                    ioExpressionType
    )
    <&> \case
      Left (UnknownError unknownError) ->
        printf "Unknown error:\n%s" unknownError
      Left (WontCompile ghcErrors) ->
        printf "Wont compile:\n%s" $ ghcErrorsToString ghcErrors
      Left (NotAllowed notAllowed) ->
        printf "Not allowed:\n%s" notAllowed
      Left (GhcException ghcException) ->
        printf "GHC exception:\n%s" ghcException
      Right s -> s

-- | Run a test that compares a golden file to the output of running some
-- Haskell expression that uses transformers, logging any type checking errors.
-- The Haskell expression should give a term of type @'IO' a@, where we have
-- @'Show' a@.
--
-- The location of the golden and log files is
-- @PACKAGE_DIR\/test\/golden\/TEST_NAME.{log,golden}@, where @PACKAGE_DIR@ is
-- the directory of the package that is being run and @TEST_NAME@ is the given
-- name of the test.
transformerGoldenTest ::
  -- | The name of the test, which is also used to determine the location of
  -- files.
  String ->
  -- | Modules that should be imported during evaluation of the expression.
  [Import] ->
  -- | A Haskell expression that gives a term of type @'IO' a@, where we have
  -- @'IO' a@.
  Expression ->
  TestTree
transformerGoldenTest testName imports ioExpression =
  goldenTest
    testName
    $ \logFile ->
      writeFile logFile . addNewline =<< testHaskell imports ioExpression
