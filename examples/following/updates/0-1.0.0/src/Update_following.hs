module Update_following () where

import EntryPoint_following
import Foreign
import Lowarn
import Lowarn.ExampleProgram.Following
import System.IO

transformer :: Transformer (Handle, Handle) State
transformer = Transformer $
  \(inHandle, outHandle) ->
    return $ Just $ State [] inHandle outHandle

foreign export ccall "hs_update_v0_v1v0v0"
  hsUpdate :: IO (StablePtr (Update (Handle, Handle) State))

hsUpdate :: IO (StablePtr (Update (Handle, Handle) State))
hsUpdate = newStablePtr $ Update transformer entryPoint
