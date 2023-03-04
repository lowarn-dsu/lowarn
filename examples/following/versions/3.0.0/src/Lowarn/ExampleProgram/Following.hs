{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Lowarn.ExampleProgram.Following
  ( User (..),
    State (..),
    eventLoop,
    showUser,
  )
where

import Control.DeepSeq
import qualified GHC.Generics as GHC (Generic)
import Lowarn
import Lowarn.Inject
import Lowarn.Transformer (Generic, HasDatatypeInfo)
import System.IO
import Text.Regex.TDFA

newtype User = User
  { userTag :: String
  }
  deriving (GHC.Generic, NFData, Generic, HasDatatypeInfo)

data State = State
  { stateUsers :: [User],
    stateIn :: Handle,
    stateOut :: Handle
  }
  deriving (GHC.Generic, Generic, HasDatatypeInfo)

instance NFData State where
  rnf state = rnf $ stateUsers state

showUser :: User -> String
showUser = userTag

eventLoop :: State -> IO State
eventLoop state@State {..} = do
  continue <- isUpdateAvailable =<< injectedRuntimeData
  if not continue
    then do
      hPutStrLn stateOut "Following:"
      mapM_ (hPutStrLn stateOut . showUser) $ reverse stateUsers
      hPutStrLn stateOut "------"
      user <- User <$> getTag
      eventLoop $ state {stateUsers = user : stateUsers}
    else return state
  where
    getTag :: IO String
    getTag = do
      hPutStrLn stateOut "Input tag of user to follow:"
      hFlush stateOut
      tag <- hGetLine stateIn
      if tag =~ "\\`[a-zA-Z]+#[0-9]{4}\\'"
        then return tag
        else hPutStrLn stateOut "Invalid tag, try again." >> getTag
