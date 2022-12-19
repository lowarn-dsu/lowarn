module Following
  ( versionNumber0,
    versionNumber1,
    versionNumber2,
    versionNumber3,
    followingProgramName,
    followingVersionId,
    followingTransformerId,
  )
where

import Data.Maybe (fromJust)
import Lowarn.ParserCombinators (runParser)
import Lowarn.ProgramName (ProgramName (ProgramName))
import Lowarn.TransformerId (TransformerId (TransformerId))
import Lowarn.VersionId (VersionId (VersionId))
import Lowarn.VersionNumber (VersionNumber, parseWithDots)

mkVersionNumber :: String -> VersionNumber
mkVersionNumber = fromJust . runParser parseWithDots

versionNumber0 :: VersionNumber
versionNumber0 = mkVersionNumber "0"

versionNumber1 :: VersionNumber
versionNumber1 = mkVersionNumber "1.0.0"

versionNumber2 :: VersionNumber
versionNumber2 = mkVersionNumber "2.0.0"

versionNumber3 :: VersionNumber
versionNumber3 = mkVersionNumber "3.0.0"

followingProgramName :: ProgramName
followingProgramName = ProgramName "following"

followingVersionId :: VersionNumber -> VersionId
followingVersionId = VersionId followingProgramName

followingTransformerId :: (VersionNumber, VersionNumber) -> TransformerId
followingTransformerId (previousVersionNumber, nextVersionNumber) =
  TransformerId followingProgramName previousVersionNumber nextVersionNumber
