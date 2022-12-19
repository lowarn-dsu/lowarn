module Main (main) where

import Control.Monad (void)
import Data.Maybe (fromJust)
import Lowarn.ParserCombinators (readWithParser)
import Lowarn.ProgramName (ProgramName, mkProgramName)
import Lowarn.Runtime
  ( loadTransformer,
    loadVersion,
    runRuntime,
    updatePackageDatabase,
  )
import Lowarn.TransformerId (TransformerId (TransformerId))
import Lowarn.VersionId (VersionId (VersionId))
import Lowarn.VersionNumber (VersionNumber, parseWithDots)

mkVersionNumber :: String -> VersionNumber
mkVersionNumber = fromJust . readWithParser parseWithDots

versionNumber1 :: VersionNumber
versionNumber1 = mkVersionNumber "1.0.0"

versionNumber2 :: VersionNumber
versionNumber2 = mkVersionNumber "2.0.0"

versionNumber3 :: VersionNumber
versionNumber3 = mkVersionNumber "3.0.0"

followingProgramName :: ProgramName
followingProgramName = fromJust $ mkProgramName "following"

followingVersionId :: VersionNumber -> VersionId
followingVersionId = VersionId followingProgramName

followingTransformerId :: (VersionNumber, VersionNumber) -> TransformerId
followingTransformerId (previousVersionNumber, nextVersionNumber) =
  TransformerId followingProgramName previousVersionNumber nextVersionNumber

main :: IO ()
main =
  runRuntime $ do
    state1 <-
      loadVersion (followingVersionId versionNumber1) Nothing

    updatePackageDatabase
    state2 <-
      loadVersion (followingVersionId versionNumber2)
        =<< loadTransformer
          (followingTransformerId (versionNumber1, versionNumber2))
          state1

    updatePackageDatabase
    void $
      loadVersion (followingVersionId versionNumber3)
        =<< loadTransformer
          (followingTransformerId (versionNumber2, versionNumber3))
          state2
