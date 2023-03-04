module Lowarn.ExampleProgram.ManualFollowing.VersionId
  ( manualFollowingVersionId_0,
    manualFollowingVersionId_1,
    manualFollowingVersionId_2,
    manualFollowingVersionId_3,
  )
where

import Lowarn.ExampleProgram.ManualFollowing.UpdateId
import Lowarn.UpdateId
import Lowarn.VersionId

manualFollowingVersionId_0 :: VersionId
manualFollowingVersionId_0 = previousVersionId manualFollowingUpdateId_0_1

manualFollowingVersionId_1 :: VersionId
manualFollowingVersionId_1 = previousVersionId manualFollowingUpdateId_1_2

manualFollowingVersionId_2 :: VersionId
manualFollowingVersionId_2 = previousVersionId manualFollowingUpdateId_2_3

manualFollowingVersionId_3 :: VersionId
manualFollowingVersionId_3 = nextVersionId manualFollowingUpdateId_2_3
