{-# LANGUAGE PackageImports #-}

module Transformer_manual_following
  ( transformer,
  )
where

import Data.Foldable (toList)
import Foreign (StablePtr, newStablePtr)
import Lowarn (Transformer (Transformer))
import qualified "lowarn-version-manual-following-v2v0v0" Lowarn.ExampleProgram.ManualFollowing as PreviousVersion
import qualified "lowarn-version-manual-following-v3v0v0" Lowarn.ExampleProgram.ManualFollowing as NextVersion

transformer :: Transformer PreviousVersion.State NextVersion.State
transformer = Transformer $
  \(PreviousVersion.State users inHandle outHandle) ->
    return $
      Just $
        NextVersion.State
          ( reverse $
              toList $
                fmap (NextVersion.User . PreviousVersion.showUser) users
          )
          inHandle
          outHandle

foreign export ccall "hs_transformer_v2v0v0_v3v0v0"
  hsTransformer ::
    IO (StablePtr (Transformer PreviousVersion.State NextVersion.State))

hsTransformer ::
  IO (StablePtr (Transformer PreviousVersion.State NextVersion.State))
hsTransformer = newStablePtr transformer
