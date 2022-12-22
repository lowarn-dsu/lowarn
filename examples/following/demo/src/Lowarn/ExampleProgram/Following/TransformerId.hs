module Lowarn.ExampleProgram.Following.TransformerId
  ( followingTransformerId_0_1,
    followingTransformerId_1_2,
    followingTransformerId_2_3,
  )
where

import Data.Maybe (fromJust)
import Lowarn.ParserCombinators (readWithParser)
import Lowarn.ProgramName (ProgramName, mkProgramName)
import Lowarn.TransformerId (TransformerId (TransformerId))
import Lowarn.VersionId (VersionId (VersionId))
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

followingProgramName :: ProgramName
followingProgramName = fromJust $ mkProgramName "following"

followingTransformerId :: (VersionNumber, VersionNumber) -> TransformerId
followingTransformerId (previousVersionNumber, nextVersionNumber) =
  TransformerId followingProgramName previousVersionNumber nextVersionNumber

followingTransformerId_0_1 :: TransformerId
followingTransformerId_0_1 =
  followingTransformerId (versionNumber0, versionNumber1)

followingTransformerId_1_2 :: TransformerId
followingTransformerId_1_2 =
  followingTransformerId (versionNumber1, versionNumber2)

followingTransformerId_2_3 :: TransformerId
followingTransformerId_2_3 =
  followingTransformerId (versionNumber2, versionNumber3)
