module Main (main) where

import Control.Monad (void)
import Lowarn.Runtime (loadProgram, runRuntime)

main :: IO ()
main =
  runRuntime . void $
    loadProgram "Lowarn.Programs.Program1" ()
      >>= loadProgram "Lowarn.Programs.Program2"
      >>= loadProgram "Lowarn.Programs.Program3"
