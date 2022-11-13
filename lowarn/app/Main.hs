module Main (main) where

import Lowarn.Runtime (loadProgram, runRuntime)

main :: IO ()
main = runRuntime $ do
  users <- loadProgram "Lowarn.Programs.Program1" ()
  () <- loadProgram "Lowarn.Programs.Program2" users
  return ()
