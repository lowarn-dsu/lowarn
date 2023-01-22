{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Transformer_following
  ( transformer,
  )
where

import Control.Arrow
import Data.Foldable (toList)
import Foreign (StablePtr, newStablePtr)
import Lowarn (Transformer)
import qualified "lowarn-version-following-v2v0v0" Lowarn.ExampleProgram.Following as PreviousVersion
import qualified "lowarn-version-following-v3v0v0" Lowarn.ExampleProgram.Following as NextVersion
import Lowarn.Transformer (Transformable (transformer))

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

foreign export ccall "hs_transformer_v2v0v0_v3v0v0"
  hsTransformer ::
    IO (StablePtr (Transformer PreviousVersion.State NextVersion.State))

hsTransformer ::
  IO (StablePtr (Transformer PreviousVersion.State NextVersion.State))
hsTransformer = newStablePtr transformer
