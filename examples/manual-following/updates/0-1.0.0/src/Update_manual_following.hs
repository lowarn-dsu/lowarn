module Update_manual_following () where

import EntryPoint_manual_following (entryPoint)
import Foreign (StablePtr, newStablePtr)
import Lowarn (Transformer (Transformer), Update (Update))
import Lowarn.ExampleProgram.ManualFollowing (State (State))
import System.IO (Handle)

transformer :: Transformer (Handle, Handle) State
transformer = Transformer $
  \(inHandle, outHandle) ->
    return $ Just $ State [] inHandle outHandle

foreign export ccall "hs_update_v0_v1v0v0"
  hsUpdate :: IO (StablePtr (Update (Handle, Handle) State))

hsUpdate :: IO (StablePtr (Update (Handle, Handle) State))
hsUpdate = newStablePtr $ Update transformer entryPoint
