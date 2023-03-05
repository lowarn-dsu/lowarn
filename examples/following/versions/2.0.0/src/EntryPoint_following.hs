{-# LANGUAGE TemplateHaskell #-}

module EntryPoint_following (entryPoint) where

import Data.Maybe
import qualified Data.Sequence as Seq
import Lowarn
import Lowarn.ExampleProgram.Following
import Lowarn.Inject
import Lowarn.TH
import System.IO

entryPoint :: EntryPoint State
entryPoint = EntryPoint $
  \runtimeData -> do
    injectRuntimeData runtimeData
    eventLoop $ fromMaybe (State Seq.empty stdin stdout) $ lastState runtimeData

entryPointExportDeclarations 'entryPoint
