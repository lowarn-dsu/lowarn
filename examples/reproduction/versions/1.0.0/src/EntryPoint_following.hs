module EntryPoint_following (entryPoint) where

import Foreign
import Lowarn
import Lowarn.ExampleProgram.Reproduction

entryPoint :: EntryPoint State
entryPoint = EntryPoint $
  \_ -> eventLoop $ State "Hello World"

foreign export ccall "hs_entryPoint_v1v0v0"
  hsEntryPoint :: IO (StablePtr (EntryPoint State))

hsEntryPoint :: IO (StablePtr (EntryPoint State))
hsEntryPoint = newStablePtr entryPoint
