module Transformer_following
  ( transformer,
  )
where

import Foreign (StablePtr, newStablePtr)
import Lowarn.ExampleProgram.Following (State (State))
import Lowarn.Runtime (Transformer (Transformer))
import System.IO (Handle)

transformer :: Transformer (Handle, Handle) State
transformer = Transformer $
  \(inHandle, outHandle) ->
    return $ Just $ State [] inHandle outHandle

foreign export ccall "hs_transformer"
  hsTransformer :: IO (StablePtr (Transformer (Handle, Handle) State))

hsTransformer :: IO (StablePtr (Transformer (Handle, Handle) State))
hsTransformer = newStablePtr transformer
