module Lowarn.ExampleProgram.ManualFollowing.UpdateId
  ( manualFollowingUpdateId_0_1,
    manualFollowingUpdateId_1_2,
    manualFollowingUpdateId_2_3,
  )
where

import Data.Maybe (fromJust)
import Lowarn.ParserCombinators (readWithParser)
import Lowarn.ProgramName (ProgramName, mkProgramName)
import Lowarn.UpdateId (UpdateId (UpdateId))
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
