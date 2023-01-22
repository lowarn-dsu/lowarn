module Transformer_manual_following
  ( transformer,
  )
where

import Foreign (StablePtr, newStablePtr)
import Lowarn (Transformer (Transformer))
import Lowarn.ExampleProgram.ManualFollowing (State (State))
import System.IO (Handle)

transformer :: Transformer (Handle, Handle) State
transformer = Transformer $
  \(inHandle, outHandle) ->
    return $ Just $ State [] inHandle outHandle

foreign export ccall "hs_transformer_v0_v1v0v0"
  hsTransformer :: IO (StablePtr (Transformer (Handle, Handle) State))

hsTransformer :: IO (StablePtr (Transformer (Handle, Handle) State))
hsTransformer = newStablePtr transformer
