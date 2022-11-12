module Main (main) where

import Lowarn.DynamicLinker (load)
import Lowarn.Types (Program (..))

loadProgram :: String -> a -> IO b
loadProgram moduleName oldState = do
  status <- load moduleName "program"
  case status of
    Just (Program p t) -> do
      let newState = t oldState
      p newState
    Nothing ->
      error ("Loading " <> moduleName <> " failed")

main :: IO ()
main = do
  users <- loadProgram "Lowarn.Programs.Program1" ()
  () <- loadProgram "Lowarn.Programs.Program2" users
  return ()
