{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module                  : Lowarn
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (GHC)
--
-- Module for interacting with Lowarn.
module Lowarn
  ( -- * Versions

    -- | Types and functions used in versions of programs.
    EntryPoint (..),
    isUpdateAvailable,
    signalUpdate,
    lastState,

    -- * Updates

    -- | Types and functions used in updates.
    Update (..),

    -- * Transformers

    -- | Types and functions used in state transformers.
    Transformer (..),

    -- * Runtime

    -- | Types and functions used by Lowarn's runtime.
    RuntimeData (RuntimeData),
    UpdateSignalRegister,
    mkUpdateSignalRegister,
    fillUpdateSignalRegister,
    UpdateInfo (UpdateInfo),
  )
where

import Control.Applicative
import Control.Arrow
import qualified Control.Category as Cat
import Control.Concurrent (MVar, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Monad
import Data.Maybe (isJust)

-- | Type for storing whether or not Lowarn should try to update a program.
newtype UpdateSignalRegister = UpdateSignalRegister
  { unUpdateSignalRegister :: MVar ()
  }

-- | Type for information about a successful update from one version of a
-- program to another.
newtype UpdateInfo a = UpdateInfo
  { -- | The state of the previous version of the program, after being
    -- transformed.
    _lastState :: a
  }

-- | Type for accessing data injected by the runtime.
data RuntimeData a = RuntimeData
  { -- | An update signal register.
    _updateSignalRegister :: UpdateSignalRegister,
    -- | If an update has occurred successfully, information about the update.
    _updateInfo :: Maybe (UpdateInfo a)
  }

-- | Type for functions that begin running a version of a program.
newtype EntryPoint a = EntryPoint
  { unEntryPoint :: RuntimeData a -> IO a
  }

-- | Type for functions that transform state from one version of a program
-- into state for another.
newtype Transformer a b = Transformer
  { unTransformer :: a -> IO (Maybe b)
  }

instance Functor (Transformer a) where
  fmap :: (b -> c) -> Transformer a b -> Transformer a c
  fmap f t = Transformer $ fmap (fmap f) . unTransformer t

instance Applicative (Transformer a) where
  pure :: b -> Transformer a b
  pure = Transformer . const . return . Just

  (<*>) :: Transformer a (b -> c) -> Transformer a b -> Transformer a c
  tf <*> t = Transformer $ \x ->
    unTransformer t x
      >>= maybe (return Nothing) (\y -> fmap ($ y) <$> unTransformer tf x)

instance Monad (Transformer a) where
  (>>=) :: Transformer a b -> (b -> Transformer a c) -> Transformer a c
  t >>= f = Transformer $ \x ->
    unTransformer t x >>= maybe (return Nothing) (\y -> unTransformer (f y) x)

instance Cat.Category Transformer where
  id :: Transformer a a
  id = Transformer $ return . Just

  (.) :: Transformer b c -> Transformer a b -> Transformer a c
  t2 . t1 =
    Transformer $ unTransformer t1 >=> maybe (return Nothing) (unTransformer t2)

instance Arrow Transformer where
  arr :: (a -> b) -> Transformer a b
  arr = Transformer . ((return . Just) .)

  first :: Transformer a b -> Transformer (a, c) (b, c)
  first t = Transformer $ \(x, y) -> fmap (,y) <$> unTransformer t x

instance Semigroup (Transformer a b) where
  (<>) :: Transformer a b -> Transformer a b -> Transformer a b
  t1 <> t2 = Transformer $ \x ->
    unTransformer t1 x >>= maybe (unTransformer t2 x) (return . Just)

instance Monoid (Transformer a b) where
  mempty = Transformer $ const $ return Nothing

instance Alternative (Transformer a) where
  empty :: Transformer a b
  empty = mempty

  (<|>) :: Transformer a b -> Transformer a b -> Transformer a b
  (<|>) = (<>)

instance MonadPlus (Transformer a)

instance ArrowZero Transformer where
  zeroArrow :: Transformer a b
  zeroArrow = mempty

instance ArrowPlus Transformer where
  (<+>) :: Transformer a b -> Transformer a b -> Transformer a b
  (<+>) = (<>)

instance ArrowChoice Transformer where
  left :: Transformer a b -> Transformer (Either a c) (Either b c)
  left t =
    Transformer $
      either (fmap (fmap Left) . unTransformer t) (return . Just . Right)

instance ArrowApply Transformer where
  app :: Transformer (Transformer a b, a) b
  app = Transformer $ uncurry unTransformer

-- | Type for functions that transform state from one version of a program
-- into state for another.
data Update a b = Update
  { _transformer :: Transformer a b,
    _entryPoint :: EntryPoint b
  }

-- | Create an update signal register.
mkUpdateSignalRegister :: IO UpdateSignalRegister
mkUpdateSignalRegister = UpdateSignalRegister <$> newEmptyMVar

-- | Fill the update signal register. A boolean is returned which is @False@ if
-- the register has already been filled, and @True@ if it hasn't.
fillUpdateSignalRegister :: UpdateSignalRegister -> IO Bool
fillUpdateSignalRegister = (`tryPutMVar` ()) . unUpdateSignalRegister

-- | Return @True@ if the runtime has a program update that can be applied.
isUpdateAvailable :: RuntimeData a -> IO Bool
isUpdateAvailable =
  (fmap isJust . tryTakeMVar) . (unUpdateSignalRegister . _updateSignalRegister)

-- | Send a signal to Lowarn that indicates that an update is available. A
-- boolean is returned which is @False@ if an update has already been signalled,
-- and @True@ if one hasn't.
signalUpdate :: RuntimeData a -> IO Bool
signalUpdate = fillUpdateSignalRegister . _updateSignalRegister

-- | Return the transformed state of the last version of the program, if there
-- was a previous version of the program and the state was able to be
-- transformed.
lastState :: RuntimeData a -> Maybe a
lastState = fmap _lastState . _updateInfo
