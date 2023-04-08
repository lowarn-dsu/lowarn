module Lowarn.ExampleProgram.ManualFollowing.DemoInfo
  ( manualFollowingProgramName,
    manualFollowingVersionId_0,
    manualFollowingVersionId_1,
    manualFollowingVersionId_2,
    manualFollowingVersionId_3,
    manualFollowingUpdateId_0_1,
    manualFollowingUpdateId_1_2,
    manualFollowingUpdateId_2_3,
  )
where

import Data.Maybe
import Lowarn.ParserCombinators
import Lowarn.ProgramName
import Lowarn.UpdateId
import Lowarn.VersionId
import Lowarn.VersionNumber

mkVersionNumberFromString :: String -> VersionNumber
mkVersionNumberFromString = fromJust . readWithParser parseWithDots

versionNumber0 :: VersionNumber
versionNumber0 = mkVersionNumberFromString "0"

versionNumber1 :: VersionNumber
versionNumber1 = mkVersionNumberFromString "1.0.0"

versionNumber2 :: VersionNumber
versionNumber2 = mkVersionNumberFromString "2.0.0"

versionNumber3 :: VersionNumber
versionNumber3 = mkVersionNumberFromString "3.0.0"

manualFollowingProgramName :: ProgramName
manualFollowingProgramName = fromJust $ mkProgramName "manual-following"

manualFollowingUpdateId :: (VersionNumber, VersionNumber) -> UpdateId
manualFollowingUpdateId (previousVersionNumber, nextVersionNumber) =
  UpdateId
    manualFollowingProgramName
    previousVersionNumber
    nextVersionNumber

manualFollowingUpdateId_0_1 :: UpdateId
manualFollowingUpdateId_0_1 =
  manualFollowingUpdateId (versionNumber0, versionNumber1)

manualFollowingUpdateId_1_2 :: UpdateId
manualFollowingUpdateId_1_2 =
  manualFollowingUpdateId (versionNumber1, versionNumber2)

manualFollowingUpdateId_2_3 :: UpdateId
manualFollowingUpdateId_2_3 =
  manualFollowingUpdateId (versionNumber2, versionNumber3)

manualFollowingVersionId_0 :: VersionId
manualFollowingVersionId_0 = previousVersionId manualFollowingUpdateId_0_1

manualFollowingVersionId_1 :: VersionId
manualFollowingVersionId_1 = previousVersionId manualFollowingUpdateId_1_2

manualFollowingVersionId_2 :: VersionId
manualFollowingVersionId_2 = previousVersionId manualFollowingUpdateId_2_3

manualFollowingVersionId_3 :: VersionId
manualFollowingVersionId_3 = nextVersionId manualFollowingUpdateId_2_3
