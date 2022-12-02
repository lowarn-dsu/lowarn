{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Lowarn.Runtime
  ( Runtime,
    Program (..),
    RuntimeData,
    runRuntime,
    loadProgram,
    liftIO,
    liftLinker,
    isUpdateAvailable,
    lastState,
  )
where

import Control.Concurrent (MVar, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Maybe (isJust)
import Lowarn.DynamicLinker (Linker, liftIO, load, runLinker)
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR2)

newtype UpdateSignal = UpdateSignal
  { unUpdateSignal :: MVar ()
  }

newtype Runtime a = Runtime
  { unRuntime :: ReaderT UpdateSignal IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

data Program a b = Program
  { _entryPoint :: RuntimeData a -> IO a,
    _transformer :: b -> IO (Maybe a)
  }

data RuntimeData a = RuntimeData
  { _updateSignal :: UpdateSignal,
    _updateInfo :: Maybe (UpdateInfo a)
  }

data UpdateInfo a = UpdateInfo
  { _label :: Maybe String,
    _lastState :: a
  }

runRuntime :: Runtime a -> IO a
runRuntime runtime = do
  updateSignal <- newEmptyMVar
  previousSignalHandler <-
    installHandler
      sigUSR2
      (Catch (void $ tryPutMVar updateSignal ()))
      Nothing
  output <- runReaderT (unRuntime runtime) $ UpdateSignal updateSignal
  liftIO $ void $ installHandler sigUSR2 previousSignalHandler Nothing
  return output

loadProgram :: String -> a -> Runtime b
loadProgram moduleName lastState_ = Runtime $ do
  updateSignal <- ask
  liftIO $
    runLinker (load moduleName "program")
      >>= maybe
        (error ("Loading " <> moduleName <> " failed"))
        ( \program -> do
            updateInfo <-
              fmap (UpdateInfo Nothing)
                <$> _transformer program lastState_
            _entryPoint program $ RuntimeData updateSignal updateInfo
        )

liftLinker :: Linker a -> Runtime a
liftLinker = Runtime . liftIO . runLinker

isUpdateAvailable :: RuntimeData a -> IO Bool
isUpdateAvailable =
  (fmap isJust . tryTakeMVar) . (unUpdateSignal . _updateSignal)

lastState :: RuntimeData a -> Maybe a
lastState = fmap _lastState . _updateInfo
