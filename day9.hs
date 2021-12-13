{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
import Data.Maybe (listToMaybe, maybeToList, mapMaybe)
import Numeric.Natural (Natural)
import Control.Monad (join, guard)
import Safe (atMay)
import Data.Bifunctor (second, first)
import Data.Foldable (minimumBy)
import Data.List.Extra (minimumOn)
import Data.List (nub, genericLength, sort)

type Nat = Natural

zip2d :: [[a]] -> [[b]] -> [[(a, b)]]
zip2d = zipWith zip

adjacent :: forall a. [[a]] -> [[(a, [a])]]
adjacent xs = combine $ xs `zip2d` shiftedUp `zip2d` shiftedDown `zip2d` shiftedLeft `zip2d` shiftedRight
  where
    dropLast = reverse . drop 1 . reverse
    len = maybe 0 length $ listToMaybe xs
    nothingRow :: [Maybe a]
    nothingRow = replicate len Nothing
    shiftedUp :: [[Maybe a]]
    shiftedUp   = map (map Just) (drop 1 xs) ++ [nothingRow]
    shiftedDown :: [[Maybe a]]
    shiftedDown = nothingRow : map (map Just) (dropLast xs)
    shiftedRight = map ((Nothing :) . dropLast . map Just) xs
    shiftedLeft = map ((++ [Nothing]) . drop 1 . map Just) xs

    combine = map (map arrange)
    arrange ((((a, x), y), z), w) = (a, maybeToList x <> maybeToList y <> maybeToList z <> maybeToList w)

solve :: [[Nat]] -> Nat
solve = sum . map ((+1) . fst) . filter (uncurry $ all . (<)) . join . adjacent

atIndex2d :: (Int, Int) -> [[a]] -> Maybe a
atIndex2d (x, y) xs = do
    xs' <- atMay xs y
    atMay xs' x

findBasin :: [[Nat]] -> (Int, Int) -> Maybe (Int, Int)
findBasin heightMap index = do
    x <- atIndex2d index heightMap
    guard (x /= 9)

    let coordR = first (+1) index
        coordL = first (subtract 1) index
        coordT = second (subtract 1) index
        coordB = second (+1) index

        r = atIndex2d coordR heightMap
        l = atIndex2d coordL heightMap
        t = atIndex2d coordT heightMap
        b = atIndex2d coordB heightMap

    let destCoord = snd $ minimumOn fst $
            (x, Nothing) : map (second Just) (concatMap maybeFstToList
                [(r, coordR), (l, coordL), (t, coordT), (b, coordB)]
                )

    case destCoord of
        Nothing -> Just index -- Found the low-point
        Just d -> findBasin heightMap d
  where
    maybeFstToList (Nothing, a) = []
    maybeFstToList (Just x, a) = [(x, a)]

infixr .:
(.:) = (.) . (.)
count = genericLength .: filter . (==)

solve2 :: [[Nat]] -> Nat
solve2 heightMap =
    let basins = mapMaybe (findBasin heightMap)
            [(x, y) | x <- [0..99], y <- [0..99]]

        countMap :: [Nat]
        countMap = map (`count` basins) $ nub basins

        largests = take 3 $ reverse $ sort countMap
    in product largests

main :: IO ()
main = do
    input <- readFile "input/9.txt"
    let heightMap = map (map $ read @Nat . pure) $ lines input
    print $ solve2 heightMap
