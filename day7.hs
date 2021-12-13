{-# LANGUAGE TypeApplications #-}

import Data.List.Split (splitOn)
import Data.List (sort)
import Numeric.Natural (Natural)
import Control.Monad (join)
type Nat = Natural

dist :: Nat -> Nat -> Nat
dist x y = case compare x y of
    GT -> x - y
    EQ -> 0
    LT -> y - x

solve :: [Nat] -> Nat
solve xs =
    let alignedPosition = middleNumber $ sort xs
    in sum $ map (dist alignedPosition) xs

solve2 :: [Nat] -> Nat
solve2 xs =
    let alignedPosition = round (average xs) - 1
    in sum $ map ((\n -> n * (n + 1) `div` 2) . dist alignedPosition) xs

main :: IO ()
main = do
    input <- readFile "input/7.txt"
    let xs = map (read @Nat) $ splitOn "," input
    -- let xs = [16,1,2,0,4,2,7,1,2,14]
    -- print $ average xs
    print $ solve2 xs

average :: Real a => [a] -> Double
average xs = realToFrac (sum xs) / realToFrac (length xs)

geometricMean :: Real a => [a] -> Double
geometricMean xs =
    realToFrac (product xs)
 ** (1 / fromIntegral (length xs))

squareMean :: Real a => [a] -> Double
squareMean xs = sqrt $
    realToFrac (realToFrac $ sum $ map (join (*)) xs)
  / realToFrac (realToFrac $ length xs)

-- TODO: Make this safe
middleNumber :: [a] -> a
middleNumber xs = xs !! (length xs `div` 2)

