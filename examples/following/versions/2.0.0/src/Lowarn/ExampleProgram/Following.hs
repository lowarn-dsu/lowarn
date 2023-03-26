module Lowarn.ExampleProgram.Following
  ( User (..),
    State (..),
    eventLoop,
    showUser,
  )
where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Printf (printf)

data User = User
  { userNickname :: String,
    userInt :: Int
  }

newtype State = State
  { stateUsers :: Seq User
  }

showUser :: User -> String
showUser (User nickname int) = printf "%s#%04d" nickname int

eventLoop :: State -> IO State
eventLoop state@(State users) = do
  putStrLn "Following (2):"
  mapM_ (putStrLn . showUser) users
  putStrLn "------"
  nickname <- getNickname
  int <- getInt
  let user = User nickname int
  eventLoop state {stateUsers = users Seq.|> user}
  where
    getInput :: String -> IO String
    getInput field = do
      putStrLn $ printf "Input %s of user to follow:" field
      getLine

    getNickname :: IO String
    getNickname = getInput "nickname"

    getInt :: IO Int
    getInt = read <$> getInput "user ID"
