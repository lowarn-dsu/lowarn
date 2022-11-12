module Main (main) where

import DynamicLinker (load)
import Types (Program (..))

loadProgram :: String -> IO a
loadProgram moduleName = do
  status <- load moduleName "program"
  case status of
    Just v -> do
      return v
    Nothing ->
      error ("Loading " <> moduleName <> " failed")

main :: IO ()
main = do
  Program main1 <- loadProgram "Program1"
  () <- main1 ()
  Program main2 <- loadProgram "Program2"
  () <- main2 ()
  return ()
