module Lowarn.ExampleProgram.Following.VersionId
  ( followingVersionId_0,
    followingVersionId_1,
    followingVersionId_2,
    followingVersionId_3,
  )
where

import Lowarn.ExampleProgram.Following.UpdateId
import Lowarn.UpdateId (nextVersionId, previousVersionId)
import Lowarn.VersionId (VersionId)

followingVersionId_0 :: VersionId
followingVersionId_0 = previousVersionId followingUpdateId_0_1

followingVersionId_1 :: VersionId
followingVersionId_1 = previousVersionId followingUpdateId_1_2

followingVersionId_2 :: VersionId
followingVersionId_2 = previousVersionId followingUpdateId_2_3

followingVersionId_3 :: VersionId
followingVersionId_3 = nextVersionId followingUpdateId_2_3
