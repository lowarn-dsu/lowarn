module Transformer_following
  ( transformer,
  )
where

import Foreign (StablePtr, newStablePtr)
import Lowarn (Transformer (Transformer))
import Lowarn.ExampleProgram.Following (State (State))
import System.IO (Handle)

transformer :: Transformer (Handle, Handle) State
transformer = Transformer $
  \(inHandle, outHandle) ->
    return $ Just $ State [] inHandle outHandle

foreign export ccall "hs_transformer"
  hsTransformer :: IO (StablePtr (Transformer (Handle, Handle) State))

hsTransformer :: IO (StablePtr (Transformer (Handle, Handle) State))
hsTransformer = newStablePtr transformer
