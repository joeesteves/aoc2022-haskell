module Main where
import Data.Set (fromList, intersection, toList)
import Data.List.Split (splitEvery)

splitStringOnMiddle :: String -> (String, String)
splitStringOnMiddle str =
  let len = length str
      mid = len `div` 2
  in splitAt mid str

findRepeatedChar :: (String, String) -> Char
findRepeatedChar (str1, str2) =
  let set1 = fromList str1
      set2 = fromList str2
      myIntersection = intersection set1 set2
  in head $ toList myIntersection

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
  let input2 = splitEvery 3 input
  print $ sum $ map (charValue . findRepeatedChar . splitStringOnMiddle) input
