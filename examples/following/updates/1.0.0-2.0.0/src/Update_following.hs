{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Update_following () where

import Control.Arrow (first)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Foreign (StablePtr, newStablePtr)
import Lowarn (Transformer (Transformer), Update (Update))
import System.Random (mkStdGen, randomR)
import System.Random.Stateful (applyIOGen, newIOGenM)
import qualified "lowarn-version-following-v1v0v0" Lowarn.ExampleProgram.Following as PreviousVersion
import "lowarn-version-following-v2v0v0" EntryPoint_following (entryPoint)
import qualified "lowarn-version-following-v2v0v0" Lowarn.ExampleProgram.Following as NextVersion

transformer :: Transformer PreviousVersion.State NextVersion.State
transformer = Transformer $
  \(PreviousVersion.State users inHandle outHandle) -> do
    ioGen <- newIOGenM (mkStdGen 0)
    users' <-
      Seq.fromList
        <$> mapM
          ( \(PreviousVersion.User nickname) ->
              applyIOGen
                (first (NextVersion.User nickname) . randomR (1, 9999))
                ioGen
          )
          users
    return $ Just $ NextVersion.State users' inHandle outHandle

foreign export ccall "hs_update_v1v0v0_v2v0v0"
  hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))

hsUpdate :: IO (StablePtr (Update PreviousVersion.State NextVersion.State))
hsUpdate = newStablePtr $ Update transformer entryPoint
