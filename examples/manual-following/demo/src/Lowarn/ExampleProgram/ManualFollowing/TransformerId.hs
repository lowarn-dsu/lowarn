module Lowarn.ExampleProgram.ManualFollowing.TransformerId
  ( manualFollowingTransformerId_0_1,
    manualFollowingTransformerId_1_2,
    manualFollowingTransformerId_2_3,
  )
where

import Data.Maybe (fromJust)
import Lowarn.ParserCombinators (readWithParser)
import Lowarn.ProgramName (ProgramName, mkProgramName)
import Lowarn.TransformerId (TransformerId (TransformerId))
import Lowarn.VersionNumber (VersionNumber, parseWithDots)

mkVersionNumber :: String -> VersionNumber
mkVersionNumber = fromJust . readWithParser parseWithDots

versionNumber0 :: VersionNumber
versionNumber0 = mkVersionNumber "0"

versionNumber1 :: VersionNumber
versionNumber1 = mkVersionNumber "1.0.0"

versionNumber2 :: VersionNumber
versionNumber2 = mkVersionNumber "2.0.0"

versionNumber3 :: VersionNumber
versionNumber3 = mkVersionNumber "3.0.0"

manualFollowingProgramName :: ProgramName
manualFollowingProgramName = fromJust $ mkProgramName "manual-following"

manualFollowingTransformerId :: (VersionNumber, VersionNumber) -> TransformerId
manualFollowingTransformerId (previousVersionNumber, nextVersionNumber) =
  TransformerId
    manualFollowingProgramName
    previousVersionNumber
    nextVersionNumber

manualFollowingTransformerId_0_1 :: TransformerId
manualFollowingTransformerId_0_1 =
  manualFollowingTransformerId (versionNumber0, versionNumber1)

manualFollowingTransformerId_1_2 :: TransformerId
manualFollowingTransformerId_1_2 =
  manualFollowingTransformerId (versionNumber1, versionNumber2)

manualFollowingTransformerId_2_3 :: TransformerId
manualFollowingTransformerId_2_3 =
  manualFollowingTransformerId (versionNumber2, versionNumber3)
