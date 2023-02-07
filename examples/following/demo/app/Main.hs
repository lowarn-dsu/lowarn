module Main (main) where

import Control.Monad (void)
import Lowarn.ExampleProgram.Following.TransformerId
import Lowarn.ExampleProgram.Following.VersionId
import Lowarn.Runtime
  ( loadTransformerAndVersion,
    loadVersion,
    runRuntime,
    updatePackageDatabase,
  )

main :: IO ()
main =
  runRuntime $ do
    state1 <-
      loadVersion followingVersionId_1 Nothing

    updatePackageDatabase
    state2 <-
      loadTransformerAndVersion
        followingTransformerId_1_2
        followingVersionId_2
        state1

    updatePackageDatabase
    void $
      loadTransformerAndVersion
        followingTransformerId_2_3
        followingVersionId_3
        state2
