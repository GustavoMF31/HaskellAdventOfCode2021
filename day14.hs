{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications #-}

import Data.List.Split (splitOn)
import Data.Bifunctor (bimap, Bifunctor (first))
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad (join)
import Data.Foldable (traverse_)
import Data.List (nub, genericLength)
import Numeric.Natural (Natural)
import qualified Data.Map as M
type Nat = Natural

forcePair [a, b] = (a, b)
forcePair _ = error "Bad input"

pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise [x] = []
pairwise (x : y : xs) = (x, y) : pairwise (y : xs)

infixr .:
(.:) = (.) . (.)

interleave :: [a] -> [a] -> [a]
interleave [] xs = xs
interleave xs [] = xs
interleave (x : xs) (y : ys) = x : y : interleave xs ys

step :: [(String, String)] -> String -> String
step rules poly = interleave poly $ join $
    map (fromJust . flip lookup rules . uncurry (++) . bimap pure pure) $ pairwise poly

step2 :: [((Char, Char), Char)] -> [((Char, Char), Nat)] -> [((Char, Char), Nat)]
step2 rules = M.toList . M.fromListWith (+) . concatMap update
  where
    update :: ((Char, Char), Nat) -> [((Char, Char), Nat)]
    update ((a, b), n) = case lookup (a, b) rules of
        Nothing -> error "No rule found" --((a, b), n)
        Just c -> [((a, c), n), ((c, b), n)]

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f = id
applyN n f = f . applyN (n - 1) f

count = genericLength .: filter . (==)

counts xs = map ((,) <*> (`count` xs)) $ nub xs

amplitude xs = maximum xs - minimum xs

forceSingleton [x] = x
forceSingleton _ = error "Could not force singleton"

lastElement = 'C'
solution :: [((Char, Char), Nat)] -> Integer
solution pairs =
    let counts = M.fromListWith (+) $ map (first fst) pairs
        cCount = fromMaybe (error "lastElement not found") $
            M.lookup lastElement $ M.fromListWith (+) $ map (first snd) pairs
    in amplitude $ fromIntegral <$> M.insert lastElement cCount counts

main :: IO ()
main = do
    input <- readFile "input/14.txt"
    let [start, rulesStr] = splitOn "\n\n" input
        rules = map (forcePair . splitOn " -> ") $ lines rulesStr
        rules' = map (bimap forcePair forceSingleton) rules
        start' = counts $ pairwise start

    -- print start'
    -- print $ step rules start
    -- traverse_ print rules
    print $ solution $ applyN 40 (step2 rules') start'
