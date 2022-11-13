module Lowarn
  ( isUpdateAvailable,
  )
where

import Control.Concurrent (tryTakeMVar)
import Data.Maybe (isJust)
import Lowarn.Types (RuntimeData (..), UpdateSignal (..))

isUpdateAvailable :: RuntimeData a -> IO Bool
isUpdateAvailable (RuntimeData (UpdateSignal updateSignal) _) =
  isJust <$> tryTakeMVar updateSignal
