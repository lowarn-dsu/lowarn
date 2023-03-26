module EntryPoint_following (entryPoint) where

import Foreign
import Lowarn
import Lowarn.ExampleProgram.Reproduction
import System.IO

entryPoint :: EntryPoint State
entryPoint = EntryPoint $
  \_ -> eventLoop $ State ["Hello World"] stdout

foreign export ccall "hs_entryPoint_v1v0v0"
  hsEntryPoint :: IO (StablePtr (EntryPoint State))

hsEntryPoint :: IO (StablePtr (EntryPoint State))
hsEntryPoint = newStablePtr entryPoint
