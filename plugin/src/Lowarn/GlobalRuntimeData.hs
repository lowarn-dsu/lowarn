module Lowarn.GlobalRuntimeData
  ( GlobalRuntimeData,
    newGlobalRuntimeData,
    putGlobalRuntimeData,
    readGlobalRuntimeData,
  )
where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar)
import Lowarn (RuntimeData)

newtype GlobalRuntimeData a = GlobalRuntimeData
  { unGlobalRuntimeData :: MVar (RuntimeData a)
  }

newGlobalRuntimeData :: IO (GlobalRuntimeData a)
newGlobalRuntimeData = GlobalRuntimeData <$> newEmptyMVar

putGlobalRuntimeData :: GlobalRuntimeData a -> RuntimeData a -> IO ()
putGlobalRuntimeData = putMVar . unGlobalRuntimeData

readGlobalRuntimeData :: GlobalRuntimeData a -> IO (RuntimeData a)
readGlobalRuntimeData = readMVar . unGlobalRuntimeData
