module Main (main) where

import Control.Monad (void)
import Lowarn.Runtime (loadTransformer, loadVersion, runRuntime)
import System.IO (stdin, stdout)

main :: IO ()
main =
  runRuntime . void $
    loadVersion "following" "1.0.0" (stdin, stdout)
      >>= loadTransformer "following" "1.0.0" "2.0.0"
      >>= loadVersion "following" "2.0.0"
      >>= loadTransformer "following" "2.0.0" "3.0.0"
      >>= loadVersion "following" "3.0.0"
