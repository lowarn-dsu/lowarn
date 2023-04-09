module Main (main) where

import Control.Monad
import Lowarn.ExampleProgram.CustomFfi.DemoInfo
import Lowarn.Runtime

main :: IO ()
main =
  runRuntime runtime False True
  where
    runtime = void $ loadVersion customFfiVersionId_1 Nothing
