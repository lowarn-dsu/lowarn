module Main (main) where

import Control.Monad
import Lowarn.ExampleProgram.Following.UpdateId
import Lowarn.ExampleProgram.Following.VersionId
import Lowarn.Runtime

main :: IO ()
main =
  runRuntime runtime True
  where
    runtime = do
      state1 <- loadVersion followingVersionId_1 Nothing

      updateRuntimePackageDatabase
      state2 <- loadUpdate followingUpdateId_1_2 state1

      updateRuntimePackageDatabase
      void $ loadUpdate followingUpdateId_2_3 state2
