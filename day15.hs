{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}

import Control.Monad (join)
import Numeric.Natural (Natural)
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe (fromMaybe, maybe, mapMaybe, catMaybes)
import Data.Foldable (minimumBy, traverse_)
import Data.Function (on)
import Safe (atMay)
import Data.Ord (Down(Down))
import Data.Bifunctor (second)
import Debug.Trace (traceShowId)
-- import Debug.Trace (traceShow, traceShowId)

type Nat = Natural

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = do
    (dx, dy) <- [(0, 1), (1, 0), (0, -1), (-1, 0)]
    pure (x + dx, y + dy)

get2d :: [[Nat]] -> (Int, Int) -> Maybe Nat
get2d xs (x, y) = do
    line <- atMay xs y
    atMay line x

{-
risk :: [[Nat]] -> Maybe Nat
risk cave = evalState (go (0, 0) []) M.empty
  where
    go :: (Int, Int) -> [(Int, Int)] -> State (M.Map (Int, Int) Nat) (Maybe Nat)
    go (99, 99) path = pure $ Just 7 -- TODO: Correct data for my input
    go coord currentPath = do
      let around = filter (`notElem` currentPath) $ neighbors coord
          maybeCostCurrent = get2d cave coord

      case maybeCostCurrent of
        Nothing -> pure Nothing
        Just costCurrent -> do

          cache <- get @(M.Map (Int, Int) Nat)
          costs <- catMaybes <$> traverse
              (\x -> maybe (go x $ coord : currentPath) (pure . Just)
                     $ M.lookup x cache)
              around

          if null costs
              then pure Nothing
              else do
                let result = minimum costs + costCurrent
                modify $ M.insert coord result
                pure $ Just result
-}

newtype MinSoFar = MkMinSoFar { getMinSoFar :: Maybe Nat }
    deriving Show via Maybe Nat
    deriving (Eq, Ord) via Down (Maybe (Down Nat))

risk :: [[Nat]] -> Maybe Nat
risk cave = getMinSoFar $ execState (go (0, 0) 0) $ MkMinSoFar Nothing
  where
    go :: (Int, Int) -> Nat -> State MinSoFar ()
    go (9, 9) cost = modify (min $ MkMinSoFar $ Just cost)
    go coord pathCost = do
        let possibilities = mapMaybe (sequence . ((,) <*> get2d cave))
              (neighbors coord) -- TODO: Better prioritizing

        minSoFar <- traceShowId <$> get
        let coordsAndCosts = filter
                ((< minSoFar) . MkMinSoFar . Just . snd) $
                map (second (+ pathCost)) possibilities

        traverse_ (uncurry go . traceShowId) coordsAndCosts

main :: IO ()
main = do
    input <- readFile "input/15example.txt"
    print $ risk $ map (map (read @Nat . pure)) $ lines input
