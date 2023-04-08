module Lowarn.ExampleProgram.Following.DemoInfo
  ( followingProgramName,
    followingVersionId_0,
    followingVersionId_1,
    followingVersionId_2,
    followingVersionId_3,
    followingUpdateId_0_1,
    followingUpdateId_1_2,
    followingUpdateId_2_3,
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

versionNumber0, versionNumber1, versionNumber2, versionNumber3 :: VersionNumber
versionNumber0 = mkVersionNumberFromString "0"
versionNumber1 = mkVersionNumberFromString "1.0.0"
versionNumber2 = mkVersionNumberFromString "2.0.0"
versionNumber3 = mkVersionNumberFromString "3.0.0"

followingProgramName :: ProgramName
followingProgramName = fromJust $ mkProgramName "following"

followingUpdateId :: (VersionNumber, VersionNumber) -> UpdateId
followingUpdateId (previousVersionNumber, nextVersionNumber) =
  UpdateId followingProgramName previousVersionNumber nextVersionNumber

followingUpdateId_0_1, followingUpdateId_1_2, followingUpdateId_2_3 :: UpdateId
followingUpdateId_0_1 = followingUpdateId (versionNumber0, versionNumber1)
followingUpdateId_1_2 = followingUpdateId (versionNumber1, versionNumber2)
followingUpdateId_2_3 = followingUpdateId (versionNumber2, versionNumber3)

followingVersionId_0,
  followingVersionId_1,
  followingVersionId_2,
  followingVersionId_3 ::
    VersionId
followingVersionId_0 = previousVersionId followingUpdateId_0_1
followingVersionId_1 = previousVersionId followingUpdateId_1_2
followingVersionId_2 = previousVersionId followingUpdateId_2_3
followingVersionId_3 = nextVersionId followingUpdateId_2_3
