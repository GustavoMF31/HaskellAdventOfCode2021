{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List (transpose)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.Ord (Down(Down))
import Data.Bool (bool)

infixr .:
(.:) = (.) . (.)
count = length .: filter . (==)
labelWith = ((,) <*>)

frequencyBy :: Eq a => (Int -> Int -> Ordering) -> NonEmpty a -> [a] -> a
frequencyBy f elems xs = fst $ maximumBy (f `on` snd) $ fmap (labelWith $ flip count xs) elems

binaryToInt :: [Bool] -> Int
binaryToInt = go . reverse
  where
    go [] = 0
    go (True  : xs) = 1 + 2 * go xs
    go (False : xs) =     2 * go xs

solve :: Ord a => (Int -> a) -> [[Bool]] -> Int
solve f = binaryToInt
        . map (frequencyBy (compare `on` f) (True :| [False]))
        . transpose

showBin :: [Bool] -> String
showBin = concatMap $ bool "0" "1"

-- TODO: Make this a safe function
filterSequentially :: Ord a => (Int -> a) -> NonEmpty Bool -> [[Bool]] -> [Bool]
filterSequentially f keys [x] = x
filterSequentially f keys xs = mostFrequent : filterSequentially f keys filtered
  where
    filtered = map (drop 1) $ filter ((mostFrequent ==) . head) xs

    mostFrequent :: Bool
    mostFrequent = frequencyBy (compare `on` f) keys $ map head xs

main :: IO ()
main = do
    input <- readFile "input/3.txt"
    let binaryNumbers = map (map (== '1')) $ lines input
    {-
    let s1 = solve id binaryNumbers
    let s2 = solve Down binaryNumbers
    print s1
    print s2
    print $ s1 * s2
    -}
    let s1 = binaryToInt $ filterSequentially id   (False :| [True]) binaryNumbers
        s2 = binaryToInt $ filterSequentially Down (True :| [False]) binaryNumbers
    print $ s1 * s2
