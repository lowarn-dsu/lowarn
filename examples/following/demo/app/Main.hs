module Main (main) where

import Control.Monad (void)
import Lowarn.ExampleProgram.Following.UpdateId
import Lowarn.ExampleProgram.Following.VersionId
import Lowarn.Runtime
  ( loadUpdate,
    loadVersion,
    runRuntime,
    updatePackageDatabase,
  )

main :: IO ()
main =
  runRuntime runtime True
  where
    runtime = do
      state1 <- loadVersion followingVersionId_1 Nothing

      updatePackageDatabase
      state2 <- loadUpdate followingUpdateId_1_2 state1

      updatePackageDatabase
      void $ loadUpdate followingUpdateId_2_3 state2
