module Main (main) where

import DynamicLinker (load)

loadVersion :: String -> String -> IO Int
loadVersion module_ symbol = do
  status <- load module_ symbol
  case status of
    Just v -> do
      return v
    Nothing ->
      error ("Loading " <> module_ <> " failed")

main :: IO ()
main = do
  v <- loadVersion "ValueProgram" "valueOfProgram"
  putStrLn "Loading program complete."
  print v
  putStrLn "Program complete."
