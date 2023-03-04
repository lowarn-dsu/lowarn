module Lowarn.ExampleProgram.Following.UpdateId
  ( followingUpdateId_0_1,
    followingUpdateId_1_2,
    followingUpdateId_2_3,
  )
where

import Data.Maybe
import Lowarn.ParserCombinators
import Lowarn.ProgramName
import Lowarn.UpdateId
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

followingProgramName :: ProgramName
followingProgramName = fromJust $ mkProgramName "following"

followingUpdateId :: (VersionNumber, VersionNumber) -> UpdateId
followingUpdateId (previousVersionNumber, nextVersionNumber) =
  UpdateId followingProgramName previousVersionNumber nextVersionNumber

followingUpdateId_0_1 :: UpdateId
followingUpdateId_0_1 =
  followingUpdateId (versionNumber0, versionNumber1)

followingUpdateId_1_2 :: UpdateId
followingUpdateId_1_2 =
  followingUpdateId (versionNumber1, versionNumber2)

followingUpdateId_2_3 :: UpdateId
followingUpdateId_2_3 =
  followingUpdateId (versionNumber2, versionNumber3)
