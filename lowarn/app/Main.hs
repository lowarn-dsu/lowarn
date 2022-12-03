module Main (main) where

import Control.Monad (void)
import Lowarn.Runtime (loadProgram, runRuntime)
import System.IO (stdin, stdout)

main :: IO ()
main =
  runRuntime . void $
    loadProgram "Lowarn.ExamplePrograms.Following.Following1" (stdin, stdout)
      >>= loadProgram "Lowarn.ExamplePrograms.Following.Following2"
      >>= loadProgram "Lowarn.ExamplePrograms.Following.Following3"
