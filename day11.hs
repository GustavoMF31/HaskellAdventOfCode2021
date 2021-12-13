{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

import Numeric.Natural (Natural)
import Control.Monad (join)
import Data.List ((\\), nub, genericLength, iterate')
type Nat = Int

findSpotsToFlash :: [[Nat]] -> [(Nat, Nat)]
findSpotsToFlash =
    map fst . filter ((> 9) . snd)
  . join . indexedBoard

indexedBoard = zipWith zip (map (\y -> map (y,) [0..9]) [0..9])

neighbors p@(x, y) = filter (/= p) $ do
    dx <- [(+1), id, subtract 1]
    dy <- [(+1), id, subtract 1]
    pure (dx x, dy y)

infixr .:
(.:) = (.) . (.)
count = genericLength .: filter . (==)

updateAllAt :: (Nat -> Nat -> Nat) -> [(Nat, Nat)] -> [[Nat]] -> [[Nat]]
updateAllAt f spots =
    map (map (\(p, n) -> f (count p spots) n))
    . indexedBoard

increaseAllAt :: [(Nat, Nat)] -> [[Nat]] -> [[Nat]]
increaseAllAt = updateAllAt (+)

zeroAllAt :: [(Nat, Nat)] -> [[Nat]] -> [[Nat]]
zeroAllAt = updateAllAt (\n -> if n /= 0 then const 0 else id)

flash :: [(Nat, Nat)] -> [[Nat]] -> (Nat, [[Nat]])
flash alreadyFlashed xs =
    let spotsToFlash = findSpotsToFlash xs \\ alreadyFlashed
    in if null spotsToFlash
        then (genericLength $ nub alreadyFlashed, zeroAllAt alreadyFlashed xs)
        else flash (alreadyFlashed ++ spotsToFlash) $
            increaseAllAt (concatMap neighbors spotsToFlash) xs

step :: [[Nat]] -> (Nat, [[Nat]])
step = flash [] . map (map (+1))

main :: IO ()
main = do
    input <- readFile "input/11.txt"
    let xs = map (map (read @Nat . pure)) $ lines input
    -- print $ sum $ map fst $ take 101 $ iterate' (step . snd) (0, xs)
    print $ fst $ head $ filter ((== 100) . snd) $ zip [0..] $ map fst $ iterate' (step . snd) (0, xs)
    -- print xs
    -- print $ snd $ step xs
    -- print $ snd $ step $ snd $ step xs

