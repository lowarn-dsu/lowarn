{-# LANGUAGE PackageImports #-}

module Update_following () where

import Control.DeepSeq
import Control.Exception
import Foreign
import Lowarn
import qualified "lowarn-version-reproduction-v1v0v0" Lowarn.ExampleProgram.Reproduction as PreviousVersion
import "lowarn-version-reproduction-v2v0v0" EntryPoint_following
import qualified "lowarn-version-reproduction-v2v0v0" Lowarn.ExampleProgram.Reproduction as NextVersion

transformer :: Transformer PreviousVersion.State NextVersion.State
transformer = Transformer $
  \(PreviousVersion.State stateStrings stateHandle) ->
    -- evaluate $ force $ Just $ (NextVersion.State $! length stateString) $! stateHandle
    return $ Just $ (NextVersion.State $! map length stateStrings) $! stateHandle

update :: Update PreviousVersion.State NextVersion.State
update = Update transformer entryPoint

foreign export ccall "hs_update_v1v0v0_v2v0v0"
  hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))

hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))
hsUpdate = newStablePtr update
