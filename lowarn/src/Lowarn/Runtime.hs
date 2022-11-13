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
import Lowarn.DynamicLinker (Linker, liftIO, load, runLinker)
import Lowarn.Types
  ( Program (..),
    RuntimeData (..),
    UpdateInfo (..),
    UpdateSignal (..),
  )
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR2)

newtype Runtime a = Runtime (UpdateSignal -> Linker a)

instance Functor Runtime where
  fmap f (Runtime linkerReader) = Runtime $ fmap f . linkerReader

instance Applicative Runtime where
  pure = Runtime . const . pure
  (Runtime linkerReader1) <*> (Runtime linkerReader2) =
    Runtime $ \updateSignal ->
      linkerReader1 updateSignal <*> linkerReader2 updateSignal

instance Monad Runtime where
  (Runtime linkerReader1) >>= f =
    Runtime $ \updateSignal -> do
      linker1 <- linkerReader1 updateSignal
      let (Runtime linkerReader2) = f linker1
      linkerReader2 updateSignal

instance MonadIO Runtime where
  liftIO = Runtime . const . liftIO

runRuntime :: Runtime a -> IO a
runRuntime (Runtime linkerReader) = do
  updateSignal <- liftIO newEmptyMVar
  previousSignalHandler <-
    liftIO $
      installHandler
        sigUSR2
        (Catch (void $ tryPutMVar updateSignal ()))
        Nothing
  output <- runLinker . linkerReader $ UpdateSignal updateSignal
  _ <-
    liftIO $
      installHandler sigUSR2 previousSignalHandler Nothing
  return output

loadProgram :: String -> a -> Runtime b
loadProgram moduleName lastState = Runtime $
  \updateSignal -> do
    status <- load moduleName "program"
    case status of
      Just (Program program transformer) -> do
        maybeNewState <- liftIO $ transformer lastState
        let updateInfo = case maybeNewState of
              Just newState -> Just $ UpdateInfo Nothing newState
              Nothing -> Nothing
        liftIO $ program $ RuntimeData updateSignal updateInfo
      Nothing ->
        error ("Loading " <> moduleName <> " failed")

liftLinker :: Linker a -> Runtime a
liftLinker = Runtime . const
