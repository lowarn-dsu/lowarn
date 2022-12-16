{-# LANGUAGE PackageImports #-}

module Transformer_following
  ( transformer,
  )
where

import Control.Arrow (first)
import qualified Data.Sequence as Seq
import qualified "lowarn-version-following-v1v0v0" Lowarn.ExampleProgram.Following as PreviousVersion
import qualified "lowarn-version-following-v2v0v0" Lowarn.ExampleProgram.Following as NewVersion
import System.Random (mkStdGen, randomR)
import System.Random.Stateful (applyIOGen, newIOGenM)

transformer :: PreviousVersion.State -> IO (Maybe NewVersion.State)
transformer (PreviousVersion.State users in_ out) = do
  ioGen <- newIOGenM (mkStdGen 0)
  users' <-
    Seq.fromList
      <$> mapM
        ( \(PreviousVersion.User nickname) ->
            applyIOGen
              (first (NewVersion.User nickname) . randomR (1, 9999))
              ioGen
        )
        users
  return $ Just $ NewVersion.State users' in_ out
