{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Update_following () where

import Control.Arrow (first)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import "lowarn-version-following-v2v0v0" EntryPoint_following (entryPoint)
import Foreign (StablePtr, newStablePtr)
import Lowarn (Update (Update))
import qualified "lowarn-version-following-v1v0v0" Lowarn.ExampleProgram.Following as PreviousVersion
import qualified "lowarn-version-following-v2v0v0" Lowarn.ExampleProgram.Following as NextVersion
import Lowarn.Transformer (Transformable (transform))
import Lowarn.Transformer.Strict (StrictTransformable (transformer'))
import System.Random (mkStdGen, randomR)
import System.Random.Stateful (applyIOGen, newIOGenM)

instance Transformable [PreviousVersion.User] (Seq NextVersion.User) where
  transform :: [PreviousVersion.User] -> IO (Maybe (Seq NextVersion.User))
  transform users = do
    ioGen <- newIOGenM (mkStdGen 0)
    Just . Seq.fromList
      <$> mapM
        ( \(PreviousVersion.User nickname) ->
            applyIOGen
              (first (NextVersion.User nickname) . randomR (1, 9999))
              ioGen
        )
        users

foreign export ccall "hs_update_v1v0v0_v2v0v0"
  hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))

hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))
hsUpdate = newStablePtr $ Update transformer' entryPoint
