module Main (main) where

import Control.Monad (void)
import Lowarn.Runtime (loadProgram, runRuntime)
import System.IO (stdin, stdout)

main :: IO ()
main =
  runRuntime . void $
    loadProgram "Lowarn.Programs.Program1" (stdin, stdout)
      >>= loadProgram "Lowarn.Programs.Program2"
      >>= loadProgram "Lowarn.Programs.Program3"
