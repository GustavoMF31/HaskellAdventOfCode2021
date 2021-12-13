import Data.List.Split (splitOn)
import Numeric.Natural (Natural)
import Data.List (transpose, partition)
import Data.Maybe (isNothing, fromMaybe)
-- import Data.Function (on)
-- import Data.Bool (bool)
import Data.Foldable (find)
import Control.Monad (join)
import Data.Bifunctor (second)
import Debug.Trace (traceShowId, trace)

type Nat = Natural
type Board = [[Maybe Nat]]

isWon :: [[Maybe a]] -> Bool
isWon xs = wonInRow (transpose xs) || wonInRow xs
-- isWon = ((||) `on` any (all isNothing)) <*> transpose
  where
    wonInRow = any $ all isNothing

markNumber :: Nat -> Board -> Board
markNumber n = map (map mark)
  where
    mark :: Maybe Nat -> Maybe Nat
    mark Nothing = Nothing
    mark (Just n') = if n == n'
        then Nothing
        else Just n'

play :: [Nat] -> [Board] -> Maybe (Nat, Board)
play [] xs = Nothing
play (n : ns) xs =
    let filledInBoards = map (markNumber n) xs
    in case find isWon filledInBoards of 
        Nothing -> play ns filledInBoards -- If no one's won keep playing
        Just winner -> Just (n, winner)

play2 :: [Nat] -> [Board] -> Maybe (Nat, Board)
play2 [] xs = Nothing
play2 (n : ns) xs =
    let filledInBoards = map (markNumber n) xs
        (won, inProgress) = partition isWon filledInBoards
    in case inProgress of 
        [] -> case won of
            [] -> Nothing -- No boards left, that's a problem
            [x] -> Just (n, x) -- Found the board that wins last
            xs -> Nothing -- Multiple boards win last
        xs -> play2 ns $
            -- trace ((++ "\n\n") $ unlines $ map (unlines . map (unwords . map (maybe "X" show))) inProgress)
            inProgress

score :: Board -> Nat
score = sum . join . map (map $ fromMaybe 0)

(.:) = (.) . (.)

solve :: [Nat] -> [Board] -> Maybe Nat
solve = fmap (uncurry (*) . second score) .: play2

main :: IO ()
main = do
    input <- readFile "input/4.txt"
    let (numsStr : boardsStr) = splitOn "\n\n" input
        nums :: [Nat]
        nums = map read $ splitOn "," numsStr

        boards :: [Board]
        boards = map (map (map (Just . read) . words) . lines) boardsStr
        
    print $ solve nums boards
