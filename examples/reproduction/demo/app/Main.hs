module Main (main) where

import Control.Monad
import Data.Maybe
import Lowarn.ParserCombinators
import Lowarn.ProgramName
import Lowarn.Runtime
import Lowarn.UpdateId
import Lowarn.VersionId
import Lowarn.VersionNumber

mkVersionNumberFromString :: String -> VersionNumber
mkVersionNumberFromString = fromJust . readWithParser parseWithDots

versionNumber1, versionNumber2 :: VersionNumber
versionNumber1 = mkVersionNumberFromString "1.0.0"
versionNumber2 = mkVersionNumberFromString "2.0.0"

programName :: ProgramName
programName = fromJust $ mkProgramName "reproduction"

updateId :: (VersionNumber, VersionNumber) -> UpdateId
updateId (previousVersionNumber, nextVersionNumber) =
  UpdateId programName previousVersionNumber nextVersionNumber

updateId_1_2 :: UpdateId
updateId_1_2 = updateId (versionNumber1, versionNumber2)

versionId_1 :: VersionId
versionId_1 = previousVersionId updateId_1_2

main :: IO ()
main =
  runRuntime runtime True
  where
    runtime = do
      state <- loadVersion versionId_1 Nothing
      updateRuntimePackageDatabase
      void $ loadUpdate updateId_1_2 state
