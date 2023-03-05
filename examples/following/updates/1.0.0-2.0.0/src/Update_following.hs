{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Update_following () where

import Control.Arrow
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Lowarn
import Lowarn.TH
import Lowarn.Transformer
import Lowarn.Transformer.Strict
import System.Random
import System.Random.Stateful
import qualified "lowarn-version-following-v1v0v0" Lowarn.ExampleProgram.Following as PreviousVersion
import "lowarn-version-following-v2v0v0" EntryPoint_following
import qualified "lowarn-version-following-v2v0v0" Lowarn.ExampleProgram.Following as NextVersion

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

update :: Update PreviousVersion.State NextVersion.State
update = Update transformer' entryPoint

updateExportDeclarations 'update
