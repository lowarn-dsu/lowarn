module Main (main) where

import Control.Monad (void)
import Lowarn.ExampleProgram.Following.TransformerId
import Lowarn.ExampleProgram.Following.VersionId
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
      loadVersion followingVersionId_1 Nothing

    updatePackageDatabase
    state2 <-
      loadVersion followingVersionId_2
        =<< loadTransformer followingTransformerId_1_2 state1

    updatePackageDatabase
    void $
      loadVersion followingVersionId_3
        =<< loadTransformer followingTransformerId_2_3 state2
