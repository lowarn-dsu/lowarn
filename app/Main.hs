module Main (main) where

import Data.Dynamic
import System.Environment (unsetEnv)
import System.Plugins (LoadStatus (..), MakeStatus (..), Module, load, make, unload)

makeVersion :: String -> IO FilePath
makeVersion path = do
  status <- make path []
  case status of
    MakeSuccess _ o -> return o
    MakeFailure errors ->
      mapM_ putStrLn errors >> error ("Making " <> path <> " failed")

loadAndRunVersion :: FilePath -> Dynamic -> IO (Dynamic, Module)
loadAndRunVersion o inputState = do
  status <- load o [] [] "main"
  case status of
    LoadSuccess m v -> do
      outputState <- v inputState
      return (outputState, m)
    LoadFailure errors ->
      mapM_ putStrLn errors >> error ("Loading " <> show o <> " failed")

main :: IO ()
main = do
  -- Unload GHC_PACKAGE_PATH for Cabal compatibility
  unsetEnv "GHC_PACKAGE_PATH"
  -- We may also need to unset other variables: https://github.com/ghc/ghc/blob/master/hadrian/src/Environment.hs

  o1 <- makeVersion "src/Program1.hs"
  o2 <- makeVersion "src/Program2.hs"
  (nextWord, m) <- loadAndRunVersion o1 (toDyn ())
  unload m
  _ <- loadAndRunVersion o2 nextWord
  return ()
