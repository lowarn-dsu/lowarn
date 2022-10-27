module Program1
  ( main,
  )
where

main :: () -> IO String
main () = putStrLn "Hello" >> return "world"
