module Lowarn.ExampleProgram.Following (State (State), eventLoop) where

import System.IO (Handle)

data User

data State = State
  { _users :: [User],
    _in :: Handle,
    _out :: Handle
  }

eventLoop :: State -> IO State
