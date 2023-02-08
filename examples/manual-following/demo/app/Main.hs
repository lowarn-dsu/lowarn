module Main (main) where

import Control.Monad (void)
import Lowarn.ExampleProgram.ManualFollowing.UpdateId
import Lowarn.ExampleProgram.ManualFollowing.VersionId
import Lowarn.Runtime
  ( loadUpdate,
    loadVersion,
    runRuntime,
    updatePackageDatabase,
  )

main :: IO ()
main =
  runRuntime runtime False
  where
    runtime = do
      state1 <- loadVersion manualFollowingVersionId_1 Nothing

      updatePackageDatabase
      state2 <- loadUpdate manualFollowingUpdateId_1_2 state1

      updatePackageDatabase
      void $ loadUpdate manualFollowingUpdateId_2_3 state2
