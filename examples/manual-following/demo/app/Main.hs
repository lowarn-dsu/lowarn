module Main (main) where

import Control.Monad
import Lowarn.ExampleProgram.ManualFollowing.UpdateId
import Lowarn.ExampleProgram.ManualFollowing.VersionId
import Lowarn.Runtime

main :: IO ()
main =
  runRuntime runtime False
  where
    runtime = do
      state1 <- loadVersion manualFollowingVersionId_1 Nothing

      updateRuntimePackageDatabase
      state2 <- loadUpdate manualFollowingUpdateId_1_2 state1

      updateRuntimePackageDatabase
      void $ loadUpdate manualFollowingUpdateId_2_3 state2
