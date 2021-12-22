import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe, fromJust)
import Safe (atMay)
import Data.Foldable (traverse_)
import Data.Bool (bool)
import Data.List (iterate')

rollingWindow :: [a] -> [[a]]
rollingWindow xs = if length xs < 3
    then []
    else take 3 xs : rollingWindow (drop 1 xs)

rollingWindow2d :: [[a]] -> [[[a]]]
rollingWindow2d =
    map (foldl (zipWith (++)) (repeat []) . map rollingWindow)
    . rollingWindow

binaryToInt :: [Bool] -> Int
binaryToInt = go . reverse
  where
    go :: [Bool] -> Int
    go [] = 0
    go (True  : xs) = 1 + 2 * go xs
    go (False : xs) = 2 * go xs

padWith :: Bool -> [[Bool]] -> [[Bool]]
padWith b xs =
       falseLine
    :  falseLine
    :  map (\line -> b : b : line ++ [b, b]) xs
    ++ [falseLine]
    ++ [falseLine]
  where
    falseLine = replicate sideLength b
    sideLength = maybe 0 length (listToMaybe xs) + 4

upscaleWith :: Bool -> [Bool] -> [[Bool]] -> [[Bool]]
upscaleWith b algorithm image =
    map (map (fromJust . atMay algorithm . binaryToInt))
    $ rollingWindow2d $ padWith b image

count x xs = length $ filter (== x) xs

display :: [[Bool]] -> String
display = unlines . map (map (bool '.' '#'))

upscaleTwice alg = upscaleWith True alg . upscaleWith False alg

main :: IO ()
main = do
    input <- readFile "input/20.txt"
    let [algStr, imageStr] = splitOn "\n\n" input
        image = map (map (== '#')) $ lines imageStr
        alg = map (== '#') $ filter (/= '\n') algStr
        upDark  = upscaleWith False alg
        upLight = upscaleWith True alg

    print $ sum $ map (count True) $
        iterate' (upscaleTwice alg) image !! 25
    -- writeFile "displayed.txt" $ display $ up $ up image

    -- traverse_ print $ upscale alg image

