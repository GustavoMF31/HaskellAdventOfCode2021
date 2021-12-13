{-# LANGUAGE TypeApplications #-}

import Numeric.Natural (Natural)
type Nat = Natural

slidingWindow :: Nat -> [a] -> [[a]]
slidingWindow n [] = []
slidingWindow n xs = take (fromIntegral n) xs : slidingWindow n (drop 1 xs)

pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise [x] = []
pairwise (x : y : xs) = (x, y) : pairwise (y : xs)

count :: Eq a => a -> [a] -> Int
count = length .: filter . (==)
  where
    infixr .:
    (.:) = (.) . (.)

solve1 :: [Int] -> Int
solve1 = count True . map (uncurry (<)) . pairwise

solve2 :: [Int] -> Int
solve2 = count True . map (uncurry (<))
       . pairwise . map sum . slidingWindow 3

main :: IO ()
main = do
    input <- readFile "input/1.txt"
    print $ solve2 $ map (read @Int) $ lines input
