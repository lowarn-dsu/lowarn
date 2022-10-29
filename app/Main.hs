module Main (main) where

import Control.Concurrent (threadDelay)
import System.Environment (unsetEnv)
import System.Plugins (LoadStatus (..), MakeStatus (..), Module, load, makeAll)

makeVersion :: String -> IO FilePath
makeVersion path = do
  status <- makeAll path ["-isrc"]
  case status of
    MakeSuccess _ o -> return o
    MakeFailure errors ->
      mapM_ putStrLn errors >> error ("Making " <> path <> " failed")

loadVersion :: FilePath -> String -> IO (Module, IO ())
loadVersion o s = do
  status <- load o ["src"] [] ("main" <> s)
  case status of
    LoadSuccess m v -> do
      return (m, v)
    LoadFailure errors ->
      mapM_ putStrLn errors >> error ("Loading " <> show o <> " failed")

main :: IO ()
main = do
  -- Unload GHC_PACKAGE_PATH for Cabal compatibility
  unsetEnv "GHC_PACKAGE_PATH"
  -- We may also need to unset other variables: https://github.com/ghc/ghc/blob/master/hadrian/src/Environment.hs

  o3 <- makeVersion "src/Program3.hs"
  o4 <- makeVersion "src/Program4.hs"
  putStrLn "Building complete."

  (_, p3) <- loadVersion o3 "3"
  putStrLn "Loading program 3 complete."
  p3
  putStrLn "Program 3 complete."

  -- This will guarantee a segmentation fault on this version of GHC
  threadDelay 100000

  (_, p4) <- loadVersion o4 "4"
  putStrLn "Loading program 4 complete."
  p4
  putStrLn "Program 4 complete."
