module Main where
import Data.List.Split (splitOn)

main :: IO ()
main = do
  file <- readFile "input.txt"
  let input = lines file
      pairs = map (splitOn ",") input
  print pairs
