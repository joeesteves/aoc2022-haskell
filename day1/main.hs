import Data.List.Split (splitOn)
import Data.List (sortOn)

parseElve :: String -> Int
parseElve = sum . map read . lines

main :: IO ()
main = do
  file <- readFile "input.txt"
  let top3ElvesCalories = take 3 $ sortOn negate $ map parseElve $ splitOn "\n\n" file
  putStrLn $ "Top score: " ++ (show $ head top3ElvesCalories)
  putStrLn $ "3 tops scores sum: " ++ (show $ sum top3ElvesCalories)
