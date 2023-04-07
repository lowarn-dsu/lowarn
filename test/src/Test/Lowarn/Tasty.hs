-- |
-- Module                  : Test.Lowarn.Tasty
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for utilities for using Tasty.
module Test.Lowarn.Tasty (BinarySemaphore, withBinarySemaphore) where

import Control.Concurrent
import Test.Tasty

-- | A binary semaphore.
type BinarySemaphore = MVar ()

-- | Run a test that requires a binary semaphore. The @'IO' a@ action will
-- give the same semaphore every time it is run.
withBinarySemaphore :: (IO BinarySemaphore -> TestTree) -> TestTree
withBinarySemaphore = withResource (newMVar ()) (const $ return ())
