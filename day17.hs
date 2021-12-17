import Data.Foldable (traverse_)
import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import Data.List ((\\))

-- x=48..70, y=-189..-148

xStart, xEnd, yStart, yEnd :: Int
xStart = 48
xEnd =  70
yStart = -189
yEnd = -148
{-
xStart = 20
xEnd =  30
yStart = -10
yEnd = -5
-}

type Coord = (Int, Int)
type Speed = (Int, Int)

step :: Coord -> Speed -> (Coord, Speed)
step (x, y) (dx, dy) = ((x + dx, y + dy), (dx', dy - 1))
  where
    dx' = case compare dx 0 of
        EQ -> dx
        LT -> dx + 1
        GT -> dx - 1

wontEverReachTarget :: Coord -> Speed -> Bool
wontEverReachTarget (x, y) (dx, dy) =
      x > xEnd   && dx > 0
  ||  x < xStart && dx < 0
  ||  y < yStart && dy < 0 

isInTarget :: Coord -> Bool
isInTarget (x, y) =
       x >= xStart
    && x <= xEnd
    && y >= yStart
    && y <= yEnd

(.:) = (.) . (.)
mightReachTarget = not .: wontEverReachTarget

reachesTarget :: Coord -> Speed -> Bool
reachesTarget coord speed =
    isInTarget coord || 
    (mightReachTarget coord speed
       && uncurry reachesTarget (step coord speed)
    )

-- solve :: [(Int, Int)]
solve =
    let possibilities = [(dx, dy)
            | dx <- [0 .. xEnd], dy <- [yStart .. -yStart + 1]]
    in length $ filter (reachesTarget (0, 0)) possibilities

forcePair [a, b] = (a, b)
forcePair _ = error "Bad input"

{-
main :: IO ()
-- main = traverse_ print solve
main = do
    testresult <- readFile "input/17testresult.txt"

    let xs :: [(Int, Int)]
        xs = map (bimap read read . forcePair . splitOn ",") $ lines testresult

    print $ solve \\ xs 
    print $ xs \\ solve 
-}

