{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lowarn.ExampleProgram.Following
  ( User (..),
    State (..),
    eventLoop,
    showUser,
  )
where

import Control.DeepSeq
import qualified GHC.Generics as GHC (Generic)
import Lowarn (isUpdateAvailable)
import Lowarn.Inject
import Lowarn.Transformer (Generic, HasDatatypeInfo)
import System.IO
  ( Handle,
    hFlush,
    hGetLine,
    hPutStrLn,
  )
import Text.Regex.TDFA

newtype User = User
  { _tag :: String
  }
  deriving (GHC.Generic, NFData, Generic, HasDatatypeInfo)

data State = State
  { _users :: [User],
    _in :: Handle,
    _out :: Handle
  }
  deriving (GHC.Generic, Generic, HasDatatypeInfo)

instance NFData State where
  rnf state = rnf $ _users state

showUser :: User -> String
showUser = _tag

eventLoop :: State -> IO State
eventLoop state@(State users in_ out) = do
  continue <- isUpdateAvailable =<< injectedRuntimeData
  if not continue
    then do
      hPutStrLn out "Following:"
      mapM_ (hPutStrLn out . showUser) $ reverse users
      hPutStrLn out "------"
      tag <- User <$> getTag
      eventLoop $ state {_users = tag : users}
    else return state
  where
    getTag :: IO String
    getTag = do
      hPutStrLn out "Input tag of user to follow:"
      hFlush out
      tag <- hGetLine in_
      if tag =~ "\\`[a-zA-Z]+#[0-9]{4}\\'"
        then return tag
        else hPutStrLn out "Invalid tag, try again." >> getTag
