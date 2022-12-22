module Transformer_following
  ( transformer,
  )
where

import Lowarn.ExampleProgram.Following (State (State))
import Lowarn.Runtime (Transformer (Transformer))
import System.IO (Handle)

transformer :: Transformer (Handle, Handle) State
transformer = Transformer $
  \(inHandle, outHandle) ->
    return $ Just $ State [] inHandle outHandle
