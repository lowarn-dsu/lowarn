module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Control.Monad (void)
import Lowarn.ExampleProgram.Following.TransformerId
import Lowarn.ExampleProgram.Following.VersionId
import Lowarn.Runtime
  ( Runtime,
    liftIO,
    loadTransformer,
    loadVersion,
    runRuntime,
    updatePackageDatabase,
  )
import System.IO (hReady, stdin)

main :: IO ()
main =
  runRuntime $ do
    state1' <- state1''

    _ <- liftIO getLine

    state2 <- loadVersion followingVersionId_2 state1'

    updatePackageDatabase
    void $
      loadVersion followingVersionId_3
        =<< loadTransformer followingTransformerId_2_3 state2

state1'' :: Runtime (Maybe a)
state1'' = do
  state1 <-
    loadVersion followingVersionId_1 Nothing

  updatePackageDatabase
  liftIO . evaluate
    =<< ( ( loadTransformer $!
              followingTransformerId_1_2
          )
            $! state1
        )
