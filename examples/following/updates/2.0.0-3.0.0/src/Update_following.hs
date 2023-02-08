{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Update_following () where

import Control.Arrow
import Data.Foldable (toList)
import "lowarn-version-following-v3v0v0" EntryPoint_following (entryPoint)
import Foreign (StablePtr, newStablePtr)
import Lowarn (Transformer, Update (Update))
import qualified "lowarn-version-following-v2v0v0" Lowarn.ExampleProgram.Following as PreviousVersion
import qualified "lowarn-version-following-v3v0v0" Lowarn.ExampleProgram.Following as NextVersion
import Lowarn.Transformer (Transformable (transformer))
import Lowarn.Transformer.Strict (StrictTransformable (transformer'))

instance
  (Traversable t) =>
  Transformable (t PreviousVersion.User) [NextVersion.User]
  where
  transformer :: Transformer (t PreviousVersion.User) [NextVersion.User]
  transformer =
    arr $
      reverse
        . toList
        . fmap (NextVersion.User . PreviousVersion.showUser)

foreign export ccall "hs_update_v2v0v0_v3v0v0"
  hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))

hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))
hsUpdate = newStablePtr $ Update transformer' entryPoint
