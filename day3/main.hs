module Main where
import Data.Set (fromList, intersection, toList)
import Data.List.Split (chunksOf)
import Data.List (intersect)

splitStringOnMiddle :: String -> [String]
splitStringOnMiddle str =
  let len = length str
      mid = len `div` 2
      (str1, str2) = splitAt mid str
    in [str1, str2]

findRepeatedChar :: [String] -> Char
findRepeatedChar strings =
  let intersectionResult = foldr1 intersect strings
  in head intersectionResult

charValue :: Char -> Int
charValue char =
  let aZMap = zip (['a'..'z'] ++ ['A' .. 'Z']) [1..]
  in case lookup char aZMap of
    Just x -> x
    Nothing -> 0

main :: IO ()
main = do
  file <- readFile "input.txt"
  let input = lines file
  let input2 = chunksOf 3 input
  print $ sum $ map (charValue . findRepeatedChar . splitStringOnMiddle) input
  print $ sum $ map (charValue . findRepeatedChar) input2
