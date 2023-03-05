{-# LANGUAGE TemplateHaskell #-}

module Update_following () where

import EntryPoint_following
import Lowarn
import Lowarn.ExampleProgram.Following
import Lowarn.TH
import System.IO

transformer :: Transformer (Handle, Handle) State
transformer = Transformer $
  \(inHandle, outHandle) ->
    return $ Just $ State [] inHandle outHandle

update :: Update (Handle, Handle) State
update = Update transformer entryPoint

updateExportDeclarations 'update
