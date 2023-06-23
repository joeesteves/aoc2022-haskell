module Main where
import Data.List (sortBy, (\\), intersect)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

parseStrRange :: String -> [Int]
parseStrRange(strRange) =
  let first:second:[] = splitOn "-" strRange
      in [(read first)..(read second)]

main :: IO ()
main = do
  file <- readFile "input.txt"
  let input = lines file
      pairs = map (splitOn ",") input
      pairOfRanges = map (sortBy (comparing length) . map parseStrRange) pairs
      pairsFullyOverlapped = foldr (\(f:s:[]) acc -> acc + if null (f \\ s) then 1 else 0) 0 pairOfRanges
      pairsPartiallyOverlapped = foldr (\(f:s:[]) acc -> acc + if null (intersect f s) then 0 else 1) 0 pairOfRanges
  putStrLn $ "FullOverlapped: " ++ show pairsFullyOverlapped
  putStrLn $ "PartiallyOverlapped: " ++ show pairsPartiallyOverlapped
