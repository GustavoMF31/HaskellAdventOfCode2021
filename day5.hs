import Data.List.Split (splitOn)
import Numeric.Natural (Natural)
import Data.Maybe (mapMaybe)
import Data.List (genericLength)
import Control.Monad (guard, when)
import Debug.Trace (traceShowId, trace, traceM)

type Nat = Natural

-- Parsing is built with unsafe functions
readCoord :: String -> (Nat, Nat)
readCoord = forceTuple . map read . splitOn ","
  where
    forceTuple [a, b] = (a, b)
    forceTuple xs = error "Bad input coordinate for readCoord"

type Coord = (Nat, Nat)
type Segment = (Coord, Coord)
data AlignedSegment
    = Horizontal Coord Nat -- Start and size
    | Vertical Coord Nat -- same, but vertically
    | Diagonal Bool Coord Nat
    -- Carries a bool indicating if its upward or not
    deriving Show

isInSegment :: Coord -> AlignedSegment -> Bool
isInSegment (x, y) (Horizontal (x0, y0) size) =
    y == y0 && x >= x0 && x <= x0 + size
isInSegment (x, y) (Vertical (x0, y0) size) =
    x == x0 && y >= y0 && y <= y0 + size
isInSegment (x, y) (Diagonal up (x0, y0) size) =
    (if up
      then y <= y0 && y >= y0 - size
      else y >= y0 && y <= y0 + size
    )
 && x >= x0 && x <= x0 + size
 && if up
    then x + y == x0 + y0
    else toInteger x - toInteger y == toInteger x0 - toInteger y0

dist :: Nat -> Nat -> Nat
dist x y = case compare x y of
    GT -> x - y
    EQ -> 0
    LT -> y - x

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy f x y = if f x > f y then x else y

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy f x y = if f x < f y then x else y

getAligned :: Segment -> AlignedSegment
getAligned ((x0, y0), (x1, y1))
    | x0 == x1 = Vertical (x0, min y0 y1) (dist y1 y0)
    | y0 == y1 = Horizontal (min x0 x1, y0) (dist x1 x0)
    | otherwise =
        let a = minBy fst (x0, y0) (x1, y1)
            b = maxBy fst (x0, y0) (x1, y1)
        in Diagonal (snd a > snd b) a (dist x0 x1) 
        -- The problem guarantess all diagonal lines are at 45 degrees

(.:) = (.) . (.)
countSatisfying = genericLength .: filter

solve :: [AlignedSegment] -> Nat
solve alignedSegments = genericLength $ do
    y <- [0..1000]
    x <- [0..1000]
    let segmentCount = countSatisfying (isInSegment (x, y)) alignedSegments
    -- traceM (show segmentCount)
    guard $ segmentCount >= 2
    -- pure ()

main :: IO ()
main = do
    input <- readFile "input/5.txt"
    let segments :: [Segment]
        segments = map ((\[start, _, end] -> (readCoord start, readCoord end)) . words) $ lines input

        aligned = map getAligned segments

    -- print $ maximum $ concatMap (\((x0, y0), (x1, y1)) -> [x0, y0, x1, y1]) segments
    -- print aligned
    -- print $ filter (isInSegment (2,4)) aligned
    print $ solve aligned
