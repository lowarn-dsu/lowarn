module Main (main) where

import Control.Monad (void)
import Lowarn.ProgramId (ProgramId (ProgramId))
import Lowarn.ProgramName (ProgramName (ProgramName))
import Lowarn.ProgramVersion (ProgramVersion, parseWithDots)
import Lowarn.Runtime (loadTransformer, loadVersion, runRuntime)
import System.IO (stdin, stdout)

mkProgramVersion :: String -> ProgramVersion
mkProgramVersion = fromJust . listToMaybe . readP_to_S parseWithDots

version1 :: ProgramVersion
version1 = mkProgramVersion "1.0.0"

version2 :: ProgramVersion
version2 = mkProgramVersion "2.0.0"

version3 :: ProgramVersion
version3 = mkProgramVersion "3.0.0"

followingName :: ProgramName
followingName = ProgramName "following"

followingId :: ProgramVersion -> ProgramId
followingId = ProgramId followingName

main :: IO ()
main =
  runRuntime . void $
    loadVersion (followingId version1) (stdin, stdout)
      >>= loadTransformer followingName version1 version2
      >>= loadVersion (followingId version2)
      >>= loadTransformer followingName version2 version3
      >>= loadVersion (followingId version3)
