{-# LANGUAGE LambdaCase #-}

-- |
-- Module                  : Test.Lowarn.Transformer
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for testing transformers with Lowarn.
module Test.Lowarn.Transformer
  ( Statement (Statement),
    Expression (Expression),
    transformerGoldenTest,
  )
where

import Data.Functor
import Data.List (isPrefixOf)
import Language.Haskell.Interpreter
  ( GhcError,
    InterpreterError (..),
    errMsg,
    eval,
    runInterpreter,
    runStmt,
    setImportsQ,
    typeChecksWithDetails,
  )
import Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import Test.Lowarn.Golden (goldenTest)
import Test.Tasty (TestTree)
import Text.Printf (printf)

newtype Statement = Statement
  { unStatement :: String
  }

newtype Expression = Expression
  { unExpression :: String
  }

ghcErrorsToString :: [GhcError] -> String
ghcErrorsToString = unlines . map errMsg

addNewline :: String -> String
addNewline s = if last s == '\n' then s else s <> "\n"

testHaskell :: [Statement] -> Expression -> IO String
testHaskell statements ioExpression =
  runInterpreter
    ( do
        unsafeSetGhcOption "-fconstraint-solver-iterations=12"
        setImportsQ $
          [ ("Prelude", Nothing),
            ("Lowarn", Nothing),
            ("Lowarn.Transformer", Nothing)
          ]
            <> map
              (\x -> ("Test.Lowarn.Types.Types" <> x, Just x))
              ["A", "B", "C", "D", "E"]
        mapM_ (runStmt . unStatement) statements
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

transformerGoldenTest ::
  String ->
  [Statement] ->
  Expression ->
  TestTree
transformerGoldenTest testName statements ioExpression =
  goldenTest
    testName
    $ \logFile ->
      writeFile logFile . addNewline =<< testHaskell statements ioExpression
