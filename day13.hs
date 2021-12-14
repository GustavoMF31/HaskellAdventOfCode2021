{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import Data.List (nub)
import Data.Bool (bool)

parse :: String -> ([(Int, Int)], [(Bool, Int)]) 
parse str = (dots, folds)
  where
    [dotsStr, foldsStr] = splitOn "\n\n" str

    dots = map (forcePair . map (read @Int) . splitOn ",")
        $ lines dotsStr

    folds :: [(Bool, Int)]
    folds = map (bimap (== "x") read . forcePair . splitOn "=" . (!! 2) . words) $ lines foldsStr

    forcePair [a, b] = (a, b)
    forcePair _ = error "Bad input"

fold :: (Bool, Int) -> [(Int, Int)] -> [(Int, Int)]
fold (True, coord) points = flip map points $ \(x, y) ->
    if x > coord
        then (2*coord - x, y)
        else (x, y)
fold (False, coord) points = flip map points $ \(x, y) ->
    if y > coord
        then (x, 2*coord - y)
        else (x, y)

display :: [(Int, Int)] -> String
display dots =
    let maxX = maximum $ map fst dots
        maxY = maximum $ map snd dots
        indices = map (\y -> map (,y) [0..maxX]) [0..maxY]
    in unlines $ flip map indices $ map (bool '.' '#' . (`elem` dots))

main :: IO ()
main = do
    input <- readFile "input/13.txt"
    let (dots, folds) = parse input

    -- print $ length $ nub dots
    -- print $ length $ nub $ fold (head folds) dots
    let folded :: [(Int, Int)]
        folded = nub $ foldl (flip fold) dots folds

    writeFile "13output.txt" $ display folded
