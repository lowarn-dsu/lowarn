module Lowarn.ExampleProgram.ManualFollowing.VersionId
  ( manualFollowingVersionId_0,
    manualFollowingVersionId_1,
    manualFollowingVersionId_2,
    manualFollowingVersionId_3,
  )
where

import Lowarn.ExampleProgram.ManualFollowing.TransformerId
import Lowarn.TransformerId (nextVersionId, previousVersionId)
import Lowarn.VersionId (VersionId)

manualFollowingVersionId_0 :: VersionId
manualFollowingVersionId_0 = previousVersionId manualFollowingTransformerId_0_1

manualFollowingVersionId_1 :: VersionId
manualFollowingVersionId_1 = previousVersionId manualFollowingTransformerId_1_2

manualFollowingVersionId_2 :: VersionId
manualFollowingVersionId_2 = previousVersionId manualFollowingTransformerId_2_3

manualFollowingVersionId_3 :: VersionId
manualFollowingVersionId_3 = nextVersionId manualFollowingTransformerId_2_3
