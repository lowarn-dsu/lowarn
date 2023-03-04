{-# LANGUAGE PackageImports #-}

module Update_manual_following () where

import Control.Arrow
import qualified Data.Sequence as Seq
import Foreign
import Lowarn
import System.Random
import System.Random.Stateful
import qualified "lowarn-version-manual-following-v1v0v0" Lowarn.ExampleProgram.ManualFollowing as PreviousVersion
import "lowarn-version-manual-following-v2v0v0" EntryPoint_manual_following
import qualified "lowarn-version-manual-following-v2v0v0" Lowarn.ExampleProgram.ManualFollowing as NextVersion

transformer :: Transformer PreviousVersion.State NextVersion.State
transformer = Transformer $
  \(PreviousVersion.State users inHandle outHandle) -> do
    ioGen <- newIOGenM (mkStdGen 0)
    transformedUsers <-
      Seq.fromList
        <$> mapM
          ( \(PreviousVersion.User nickname) ->
              applyIOGen
                (first (NextVersion.User nickname) . randomR (1, 9999))
                ioGen
          )
          users
    return $ Just $ NextVersion.State transformedUsers inHandle outHandle

foreign export ccall "hs_update_v1v0v0_v2v0v0"
  hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))

hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))
hsUpdate = newStablePtr $ Update transformer entryPoint
