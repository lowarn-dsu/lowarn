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
  runRuntime runtime True
  where
    runtime = do
      state1 <- loadVersion followingVersionId_1 Nothing

      updatePackageDatabase
      state2 <- loadTransformerAndVersion followingTransformerId_1_2 state1

      updatePackageDatabase
      void $ loadTransformerAndVersion followingTransformerId_2_3 state2
