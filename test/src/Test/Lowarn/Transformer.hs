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
  ( eval,
    runInterpreter,
    runStmt,
    setImportsQ,
    typeChecksWithDetails,
  )
import Test.Lowarn.Golden (goldenTest)
import Test.Tasty (TestTree)
import Text.Printf (printf)

newtype Statement = Statement
  { unStatement :: String
  }

newtype Expression = Expression
  { unExpression :: String
  }

testHaskell :: [Statement] -> Expression -> IO String
testHaskell statements ioExpression =
  runInterpreter
    ( do
        setImportsQ
          [ ("Prelude", Nothing),
            ("Lowarn", Nothing),
            ("Lowarn.Transformer", Nothing),
            ("Test.Lowarn.Types.TypesA", Just "A"),
            ("Test.Lowarn.Types.TypesB", Just "B"),
            ("Test.Lowarn.Types.TypesC", Just "C"),
            ("Test.Lowarn.Types.TypesD", Just "D")
          ]
        mapM_ (runStmt . unStatement) statements
        typeChecksWithDetails (unExpression ioExpression) >>= \case
          Left e -> return $ printf "Type checking error: %s" (show e)
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
      Left e -> printf "Error: %s" (show e)
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
      writeFile logFile . (<> "\n") =<< testHaskell statements ioExpression
