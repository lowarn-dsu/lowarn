module Lowarn.ExampleProgram.Following
  ( User (..),
    State (..),
    eventLoop,
    showUser,
  )
where

newtype User = User
  { userName :: String
  }

newtype State = State
  { stateUsers :: [User]
  }

showUser :: User -> String
showUser = userName

eventLoop :: State -> IO State
eventLoop (State users) = do
  putStrLn "Following (1):"
  mapM_ (putStrLn . showUser) users
  putStrLn "------"
  return $ State [User "a"]
