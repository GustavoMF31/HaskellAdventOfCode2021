{-# LANGUAGE TypeApplications, DerivingVia #-}
{-# LANGUAGE TupleSections #-}

import Numeric.Natural (Natural)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Ord (Down(Down))
import Data.Maybe (fromMaybe, mapMaybe)
import Safe (atMay)
import Data.Bifunctor (second)
import Control.Arrow ((&&&))
type Nat = Natural

newtype MinSoFar = MkMinSoFar { getMinSoFar :: Maybe Nat }
    deriving Show via Maybe Nat
    deriving (Eq, Ord) via Down (Maybe (Down Nat))

unreachable :: MinSoFar
unreachable = MkMinSoFar Nothing

neighbors :: (Nat, Nat) -> [(Nat, Nat)]
neighbors (x, y) =
       [(x - 1, y) | x > 0]
    ++ [(x, y - 1) | y > 0]
    ++ [(x, y + 1), (x + 1, y)]

(.:) = (.) . (.)
knownCost :: (Nat, Nat) -> Map (Nat, Nat) MinSoFar -> MinSoFar
knownCost = fromMaybe (MkMinSoFar Nothing) .: M.lookup

exploreNeighbors :: [[Nat]] -> ((Nat, Nat), MinSoFar) -> Map (Nat, Nat) MinSoFar
exploreNeighbors cavern (coord, currentCost) =
    let neighborsWithCosts = mapMaybe (\n -> (n,) <$> lookupCost n cavern) $ neighbors coord
        addPathCost = case currentCost of
            MkMinSoFar Nothing -> const unreachable
            MkMinSoFar (Just c) -> MkMinSoFar . Just . (+ c)
    in M.fromList $ map (second addPathCost) neighborsWithCosts

lookupCost :: (Nat, Nat) -> [[a]] -> Maybe a
lookupCost (x, y) xs = do
    line <- atMay xs (fromIntegral y)
    atMay line (fromIntegral x)

pathCost :: [[Nat]] -> MinSoFar
pathCost cavern = go (M.singleton (0, 0) (MkMinSoFar $ Just 0)) [(0, 0)]
  where
    go :: Map (Nat, Nat) MinSoFar -> [(Nat, Nat)] -> MinSoFar
    go explored [] = knownCost (499, 499) explored
    go explored frontier =
        let newCosts :: Map (Nat, Nat) MinSoFar
            newCosts = M.unionsWith min $ map (exploreNeighbors cavern . (id &&& flip knownCost explored)) frontier

            newExplored = M.unionWith min explored newCosts

            newFrontier :: [(Nat, Nat)]
            newFrontier =
                filter (\c -> knownCost c newExplored /= knownCost c explored)
                $ M.keys newCosts

        in go newExplored newFrontier

fullMap :: [[Nat]] -> [[Nat]]
fullMap original = concatMap
    (\n -> foldl (zipWith (++)) (repeat []) $ take 5 $ drop n increasingMap)
    [0..4]
  where
    increasingMap :: [ [[Nat]] ]
    increasingMap = iterate increaseAll original

    increaseAll :: [[Nat]] -> [[Nat]]
    increaseAll = map $ map $ \x -> if x == 9 then 1 else x + 1

main :: IO ()
main = do
    input <- readFile "input/15.txt"
    let cavern = map (map (read @Nat . pure)) $ lines input

    print $ pathCost $ fullMap cavern
