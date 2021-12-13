{-# LANGUAGE TypeApplications, NoMonomorphismRestriction #-}

import Numeric.Natural
import Data.List.Split (splitOn)
import Data.List (iterate', genericLength)
import Data.Bifunctor (first)
import qualified Data.Map as M

type Nat = Natural

-- Avoids crashes due to nat underflow
tryMinus :: Nat -> Nat -> Maybe Nat
tryMinus x y
    | x >= y = Just $ x - y
    | otherwise = Nothing

-- Maps from time left until reproduction to fish count
type FishPopulation = M.Map Nat Nat

infixr .:
(.:) = (.) . (.)
occurences = genericLength .: filter . (==)

step :: FishPopulation -> FishPopulation
step xs =
    let stepped = map (first (`tryMinus` 1)) $ M.toList xs
        fishUpdates :: [FishPopulation]
        fishUpdates = map fishUpdate stepped
    in M.unionsWith (+) fishUpdates

fishUpdate :: (Maybe Nat, Nat) -> FishPopulation
fishUpdate (Nothing, n) = M.fromList [(6, n), (8, n)]
fishUpdate (Just x, n) = M.fromList [(x, n)]

countFish :: FishPopulation -> Nat
countFish = sum . map snd . M.toList

main :: IO ()
main = do
    input <- readFile "input/6.txt"
    let fish :: [Nat]
        fish = map (read @Nat) $ splitOn "," input
    -- print $ length $ iterate' step fish !! 80

    print
        $ countFish $ iterate' step (M.fromList
        $ map (\x -> (x, occurences x fish)) [0 :: Nat .. 5 :: Nat]
        ) !! 256
