module Main (main) where

import Control.Monad (void)
import Data.Maybe (fromJust, listToMaybe)
import Lowarn.ProgramId (ProgramId (ProgramId))
import Lowarn.ProgramName (ProgramName (ProgramName))
import Lowarn.ProgramVersion (ProgramVersion, parseWithDots)
import Lowarn.Runtime
  ( loadTransformer,
    loadVersion,
    runRuntime,
    updatePackageDatabase,
  )
import Text.ParserCombinators.ReadP (eof, readP_to_S)

mkProgramVersion :: String -> ProgramVersion
mkProgramVersion =
  fst
    . fromJust
    . listToMaybe
    . readP_to_S
      (parseWithDots <* eof)

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
  runRuntime $ do
    state1 <-
      loadVersion (followingId version1) Nothing

    updatePackageDatabase
    state2 <-
      loadVersion (followingId version2)
        =<< loadTransformer followingName (version1, version2) state1

    updatePackageDatabase
    void $
      loadVersion (followingId version3)
        =<< loadTransformer followingName (version2, version3) state2
