{-# LANGUAGE PackageImports #-}

module Transformer_following
  ( transformer,
  )
where

import Data.Foldable (toList)
import qualified "lowarn-version-following-v2v0v0" Lowarn.ExampleProgram.Following as PreviousVersion
import qualified "lowarn-version-following-v3v0v0" Lowarn.ExampleProgram.Following as NextVersion

transformer :: PreviousVersion.State -> IO (Maybe NextVersion.State)
transformer (PreviousVersion.State users in_ out) =
  return $ Just $ NextVersion.State users' in_ out
  where
    users' =
      reverse $
        toList $
          fmap (NextVersion.User . PreviousVersion.showUser) users
