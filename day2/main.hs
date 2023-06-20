import Data.List.Split (splitOn)

data OPT = R | P | S deriving (Show, Eq, Enum, Bounded)

nextElement :: OPT -> OPT
nextElement x
  | x == maxBound = minBound
  | otherwise = succ x

prevElement :: OPT -> OPT
prevElement x
  | x == minBound = maxBound
  | otherwise = pred x

instance Read OPT where
  readsPrec _ input = case input of
    "A" -> [(R, "")]
    "X" -> [(R, "")]
    "B" -> [(P, "")]
    "Y" -> [(P, "")]
    "C" -> [(S, "")]
    "Z" -> [(S, "")]
    _   -> []

instance Ord OPT where
  compare R R = EQ
  compare R P = LT
  compare R S = GT
  compare P P = EQ
  compare P S = LT
  compare P R = GT
  compare S S = EQ
  compare S R = LT
  compare S P = GT

materialPoint :: OPT -> Int
materialPoint opt = case opt of
  R -> 1
  P -> 2
  S -> 3

parseOpt :: String -> [OPT]
parseOpt = (map read) .  splitOn " "

changeOpt :: [OPT] -> [OPT]
changeOpt [d, R] = [d, prevElement d]
changeOpt [d, P] = [d, d]
changeOpt [d, S] = [d, nextElement d]

parseMatch :: [OPT] -> Int
parseMatch [opt1, opt2] =
  let result = case (opt2 `compare` opt1) of
        LT -> 0
        GT -> 6
        EQ -> 3
  in result + materialPoint opt2

main :: IO ()
main = do
  file <- readFile "input.txt"
  let input = lines file
      result1 = map (parseMatch . parseOpt) input
      result2 = map (parseMatch . changeOpt . parseOpt) input
  putStrLn $ "Sum scores: " ++ (show $ sum result1)
  putStrLn $ "Sum realistic score: " ++ (show $ sum result2)
