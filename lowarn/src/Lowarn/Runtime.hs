module Lowarn.Runtime (loadProgram) where

import Lowarn.DynamicLinker (load)
import Lowarn.Types (Program (..))

loadProgram :: String -> a -> IO b
loadProgram moduleName state = do
  status <- load moduleName "program"
  case status of
    Just (Program program transformer) ->
      program =<< transformer state
    Nothing ->
      error ("Loading " <> moduleName <> " failed")
