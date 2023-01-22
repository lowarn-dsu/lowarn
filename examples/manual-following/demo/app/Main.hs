module Main (main) where

import Control.Monad (void)
import Lowarn.ExampleProgram.ManualFollowing.TransformerId
import Lowarn.ExampleProgram.ManualFollowing.VersionId
import Lowarn.Runtime
  ( loadTransformer,
    loadVersion,
    runRuntime,
    updatePackageDatabase,
  )

main :: IO ()
main =
  runRuntime $ do
    state1 <-
      loadVersion manualFollowingVersionId_1 Nothing

    updatePackageDatabase
    state2 <-
      loadVersion manualFollowingVersionId_2
        =<< loadTransformer manualFollowingTransformerId_1_2 state1

    updatePackageDatabase
    void $
      loadVersion manualFollowingVersionId_3
        =<< loadTransformer manualFollowingTransformerId_2_3 state2
