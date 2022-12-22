{-# LANGUAGE PackageImports #-}

module Transformer_following
  ( transformer,
  )
where

import Data.Foldable (toList)
import Lowarn.Runtime (Transformer (Transformer))
import qualified "lowarn-version-following-v2v0v0" Lowarn.ExampleProgram.Following as PreviousVersion
import qualified "lowarn-version-following-v3v0v0" Lowarn.ExampleProgram.Following as NextVersion

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
