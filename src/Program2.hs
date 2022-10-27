module Program2
  ( main,
  )
where

import Types

main :: User -> IO ()
main u = putStrLn $ username u
