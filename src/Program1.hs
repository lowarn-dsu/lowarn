module Program1
  ( main,
  )
where

import Types

main :: () -> IO User
main () = do
  putStrLn "Hello"
  return $ User "user" 1234
