{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lowarn.Types
  ( Program (..),
    RuntimeData (..),
    UpdateSignal (..),
    UpdateInfo (..),
  )
where

import Control.Concurrent (MVar)

data Program a b = Program
  { _program :: RuntimeData a -> IO a,
    _transformer :: b -> IO (Maybe a)
  }

data RuntimeData a = RuntimeData
  { _updateSignal :: UpdateSignal,
    _updateInfo :: Maybe (UpdateInfo a)
  }

newtype UpdateSignal = UpdateSignal (MVar ())

data UpdateInfo a = UpdateInfo
  { _label :: Maybe String,
    _lastState :: a
  }
