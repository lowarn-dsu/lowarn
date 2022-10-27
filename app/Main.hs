module Main (main) where

import Control.Monad (join)
import System.Environment (unsetEnv)
import System.Plugins (LoadStatus (..), MakeStatus (..), load, make)

makeVersion :: String -> IO FilePath
makeVersion path = do
  status <- make path []
  case status of
    MakeSuccess _ o -> return o
    MakeFailure errors ->
      mapM_ putStrLn errors >> error ("Making " <> path <> " failed")

loadAndRunVersion :: FilePath -> IO ()
loadAndRunVersion o = do
  status <- load o [] [] "main"
  () <-
    join
      ( case status of
          LoadSuccess _ v -> return v
          LoadFailure errors ->
            mapM_ putStrLn errors >> error ("Loading " <> show o <> " failed")
      )
  return ()

main :: IO ()
main = do
  -- Unload GHC_PACKAGE_PATH for Cabal compatibility
  unsetEnv "GHC_PACKAGE_PATH"
  -- We may also need to unset other variables: https://github.com/ghc/ghc/blob/master/hadrian/src/Environment.hs

  o1 <- makeVersion "src/Program1.hs"
  o2 <- makeVersion "src/Program2.hs"
  loadAndRunVersion o1
  loadAndRunVersion o2
