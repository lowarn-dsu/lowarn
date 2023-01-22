{-# LANGUAGE QuasiQuotes #-}

module Spec.Transformer () where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ

a :: String
a =
  [r|
putStrLn "Hello world!" |]

transformerTests :: TestTree
transformerTests =
  testGroup
    "Transformers"
    []
