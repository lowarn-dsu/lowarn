{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lowarn.Runtime (RuntimeMonad, runRuntimeMonad, loadProgram, liftLinkerMonad) where

import Control.Monad.IO.Class (MonadIO)
import Lowarn.DynamicLinker (LinkerMonad, liftIO, load, runLinkerMonad)
import Lowarn.Types (Program (..))

newtype RuntimeMonad a = RuntimeMonad (LinkerMonad a)
  deriving (Functor, Applicative, Monad, MonadIO)

runRuntimeMonad :: RuntimeMonad a -> IO a
runRuntimeMonad (RuntimeMonad linker) =
  runLinkerMonad $ do
    linker

loadProgram :: String -> a -> RuntimeMonad b
loadProgram moduleName state = RuntimeMonad $ do
  status <- load moduleName "program"
  case status of
    Just (Program program transformer) ->
      liftIO $ program =<< transformer state
    Nothing ->
      error ("Loading " <> moduleName <> " failed")

liftLinkerMonad :: LinkerMonad a -> RuntimeMonad a
liftLinkerMonad = RuntimeMonad
