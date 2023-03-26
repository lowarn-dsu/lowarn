{-# LANGUAGE PackageImports #-}

module Update_reproduction () where

import Foreign (StablePtr, newStablePtr)
import Lowarn (Transformer (Transformer), Update (Update))
import qualified "lowarn-version-reproduction-v1v0v0" Lowarn.ExampleProgram.Reproduction as PreviousVersion
import "lowarn-version-reproduction-v2v0v0" EntryPoint_reproduction (entryPoint)
import qualified "lowarn-version-reproduction-v2v0v0" Lowarn.ExampleProgram.Reproduction as NextVersion

transformer :: Transformer PreviousVersion.State NextVersion.State
transformer =
  Transformer $
    return . Just . NextVersion.State . PreviousVersion.unState

foreign export ccall "hs_update_v1v0v0_v2v0v0"
  hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))

hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))
hsUpdate = newStablePtr $ Update transformer entryPoint
