{-# LANGUAGE PackageImports #-}

module Update_following () where

import qualified Data.Sequence as Seq
import Foreign (StablePtr, newStablePtr)
import Lowarn (Transformer (Transformer), Update (Update))
import qualified "lowarn-version-following-v1v0v0" Lowarn.ExampleProgram.Following as PreviousVersion
import "lowarn-version-following-v2v0v0" EntryPoint_following (entryPoint)
import qualified "lowarn-version-following-v2v0v0" Lowarn.ExampleProgram.Following as NextVersion

transformer :: Transformer PreviousVersion.State NextVersion.State
transformer = Transformer $
  \(PreviousVersion.State users) ->
    return $
      Just $
        NextVersion.State $
          Seq.fromList $
            map
              ( \(PreviousVersion.User nickname) ->
                  (NextVersion.User nickname 1000)
              )
              users

foreign export ccall "hs_update_v1v0v0_v2v0v0"
  hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))

hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))
hsUpdate = newStablePtr $ Update transformer entryPoint
