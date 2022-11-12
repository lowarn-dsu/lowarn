module Main (main) where

import Lowarn.Runtime (loadProgram, runRuntimeMonad)

main :: IO ()
main = runRuntimeMonad $ do
  users <- loadProgram "Lowarn.Programs.Program1" ()
  () <- loadProgram "Lowarn.Programs.Program2" users
  return ()
