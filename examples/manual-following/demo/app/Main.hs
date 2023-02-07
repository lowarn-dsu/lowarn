module Main (main) where

import Control.Monad (void)
import Lowarn.ExampleProgram.ManualFollowing.TransformerId
import Lowarn.ExampleProgram.ManualFollowing.VersionId
import Lowarn.Runtime
  ( loadTransformerAndVersion,
    loadVersion,
    runRuntime,
    updatePackageDatabase,
  )

main :: IO ()
main =
  runRuntime $ do
    state1 <- loadVersion manualFollowingVersionId_1 Nothing

    updatePackageDatabase
    state2 <- loadTransformerAndVersion manualFollowingTransformerId_1_2 state1

    updatePackageDatabase
    void $ loadTransformerAndVersion manualFollowingTransformerId_2_3 state2
