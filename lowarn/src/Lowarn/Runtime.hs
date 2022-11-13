{-# LANGUAGE DerivingVia #-}

module Lowarn.Runtime
  ( Runtime,
    RuntimeData,
    runRuntime,
    loadProgram,
    liftIO,
    liftLinker,
  )
where

import Control.Concurrent (newEmptyMVar, tryPutMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Lowarn.DynamicLinker (Linker, liftIO, load, runLinker)
import Lowarn.Types
  ( Program (..),
    RuntimeData (..),
    UpdateInfo (..),
    UpdateSignal (..),
  )
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR2)

newtype Runtime a = Runtime (ReaderT UpdateSignal IO a)
  deriving (Functor, Applicative, Monad, MonadIO) via (ReaderT UpdateSignal IO)

runRuntime :: Runtime a -> IO a
runRuntime (Runtime reader) = do
  updateSignal <- newEmptyMVar
  previousSignalHandler <-
    installHandler
      sigUSR2
      (Catch (void $ tryPutMVar updateSignal ()))
      Nothing
  output <- runReaderT reader $ UpdateSignal updateSignal
  _ <-
    liftIO $
      installHandler sigUSR2 previousSignalHandler Nothing
  return output

loadProgram :: String -> a -> Runtime b
loadProgram moduleName lastState = Runtime $ do
  updateSignal <- ask
  liftIO $
    runLinker (load moduleName "program")
      >>= maybe
        (error ("Loading " <> moduleName <> " failed"))
        ( \(Program program transformer) -> do
            maybeNewState <- transformer lastState
            let updateInfo = case maybeNewState of
                  Just newState -> Just $ UpdateInfo Nothing newState
                  Nothing -> Nothing
            program $ RuntimeData updateSignal updateInfo
        )

liftLinker :: Linker a -> Runtime a
liftLinker = Runtime . liftIO . runLinker
