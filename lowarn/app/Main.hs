module Main (main) where

import Lowarn.Runtime (loadProgram)

main :: IO ()
main = do
  users <- loadProgram "Lowarn.Programs.Program1" ()
  () <- loadProgram "Lowarn.Programs.Program2" users
  return ()
