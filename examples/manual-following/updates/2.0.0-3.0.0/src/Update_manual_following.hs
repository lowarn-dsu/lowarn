{-# LANGUAGE PackageImports #-}

module Update_manual_following () where

import Data.Foldable
import Foreign
import Lowarn
import qualified "lowarn-version-manual-following-v2v0v0" Lowarn.ExampleProgram.ManualFollowing as PreviousVersion
import "lowarn-version-manual-following-v3v0v0" EntryPoint_manual_following
import qualified "lowarn-version-manual-following-v3v0v0" Lowarn.ExampleProgram.ManualFollowing as NextVersion

transformer :: Transformer PreviousVersion.State NextVersion.State
transformer = Transformer $
  \(PreviousVersion.State users inHandle outHandle) ->
    return $
      Just $
        NextVersion.State
          ( reverse $
              toList $
                fmap (NextVersion.User . PreviousVersion.showUser) users
          )
          inHandle
          outHandle

foreign export ccall "hs_update_v2v0v0_v3v0v0"
  hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))

hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))
hsUpdate = newStablePtr $ Update transformer entryPoint
