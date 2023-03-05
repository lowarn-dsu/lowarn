{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Update_following () where

import Control.Arrow
import Data.Foldable
import Lowarn
import Lowarn.TH
import Lowarn.Transformer
import Lowarn.Transformer.Strict
import qualified "lowarn-version-following-v2v0v0" Lowarn.ExampleProgram.Following as PreviousVersion
import "lowarn-version-following-v3v0v0" EntryPoint_following
import qualified "lowarn-version-following-v3v0v0" Lowarn.ExampleProgram.Following as NextVersion

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

update :: Update PreviousVersion.State NextVersion.State
update = Update transformer' entryPoint

updateExportDeclarations 'update
