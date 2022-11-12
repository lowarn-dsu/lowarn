module Main (main) where

import qualified Data.Sequence as Seq
import DynamicLinker (load)
import Types (Program (..))

loadProgram :: String -> IO (a -> IO b)
loadProgram moduleName = do
  status <- load moduleName "program"
  case status of
    Just (Program p) -> do
      return p
    Nothing ->
      error ("Loading " <> moduleName <> " failed")

main :: IO ()
main = do
  program1 <- loadProgram "Program1"
  users <- program1 ()
  let users' = Seq.fromList users
  program2 <- loadProgram "Program2"
  () <- program2 users'
  return ()
