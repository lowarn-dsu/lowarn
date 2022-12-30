-- |
-- Module                  : Test.Lowarn.Tasty
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for utilities for Tasty.
module Test.Lowarn.Tasty (BinarySemaphore, withBinarySemaphore) where

import Control.Concurrent (MVar, newMVar)
import Test.Tasty (TestTree, withResource)

type BinarySemaphore = MVar ()

withBinarySemaphore :: (IO BinarySemaphore -> TestTree) -> TestTree
withBinarySemaphore = withResource (newMVar ()) (const $ return ())
