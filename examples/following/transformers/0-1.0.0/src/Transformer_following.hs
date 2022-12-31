module Transformer_following
  ( transformer,
  )
where

import Lowarn (Transformer (Transformer))
import Lowarn.ExampleProgram.Following (State (State))
import System.IO (Handle)

transformer :: Transformer (Handle, Handle) State
transformer = Transformer $
  \(inHandle, outHandle) ->
    return $ Just $ State [] inHandle outHandle
