module Version_following
  ( version,
  )
where

import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Lowarn.ExampleProgram.Following (State (State), eventLoop)
import Lowarn.Runtime (Version (..), lastState)
import System.IO
  ( stdin,
    stdout,
  )

version :: Version State
version = Version $
  \runtimeData ->
    eventLoop runtimeData $
      fromMaybe (State Seq.empty stdin stdout) (lastState runtimeData)
