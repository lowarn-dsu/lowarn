{-# LANGUAGE PackageImports #-}

module Update_manual_following () where

import Control.Arrow (first)
import qualified Data.Sequence as Seq
import "lowarn-version-manual-following-v2v0v0" EntryPoint_manual_following (entryPoint)
import Foreign (StablePtr, newStablePtr)
import Lowarn (Transformer (Transformer), Update (Update))
import qualified "lowarn-version-manual-following-v1v0v0" Lowarn.ExampleProgram.ManualFollowing as PreviousVersion
import qualified "lowarn-version-manual-following-v2v0v0" Lowarn.ExampleProgram.ManualFollowing as NextVersion
import System.Random (mkStdGen, randomR)
import System.Random.Stateful (applyIOGen, newIOGenM)

transformer :: Transformer PreviousVersion.State NextVersion.State
transformer = Transformer $
  \(PreviousVersion.State users inHandle outHandle) -> do
    ioGen <- newIOGenM (mkStdGen 0)
    users' <-
      Seq.fromList
        <$> mapM
          ( \(PreviousVersion.User nickname) ->
              applyIOGen
                (first (NextVersion.User nickname) . randomR (1, 9999))
                ioGen
          )
          users
    return $ Just $ NextVersion.State users' inHandle outHandle

foreign export ccall "hs_update_v1v0v0_v2v0v0"
  hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))

hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))
hsUpdate = newStablePtr $ Update transformer entryPoint