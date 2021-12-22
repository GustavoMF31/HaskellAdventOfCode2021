import Data.List.Split (splitOn)
import Data.Foldable (traverse_, asum, Foldable (toList))
import Data.Maybe (listToMaybe, maybeToList, mapMaybe)
import Control.Monad (guard, join)
import Data.Bifunctor (Bifunctor(second))
import Debug.Trace (traceShowId, traceM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Control.Applicative ((<|>))
import qualified Data.List.NonEmpty as NE
import Data.List (nub)
import Data.Tuple (swap)

type Coord = (Int, Int, Int)

distSquared :: Coord -> Coord -> Int
distSquared (x, y, z) (x', y', z') =
    square (x - x') + square (y - y') + square (z - z')
  where
    square x = x * x

pick :: [a] -> [(a, [a])]
pick [] = []
pick (x : xs) = (x, xs) : map (second (x :)) (pick xs)

enoughMatches = 12
(.:) = (.) . (.)
match :: [Coord] -> [Coord] -> Maybe (NonEmpty (Coord, Coord))
match = listToMaybe .: go []
  where
    go :: [(Coord, Coord)] -> [Coord] -> [Coord]
       -> [NonEmpty (Coord, Coord)]
    go associationSoFar xs ys = do
        (a , xs') <- pick xs
        (a', ys') <- pick ys

        guard $
               map (distSquared a  . fst) associationSoFar
            == map (distSquared a' . snd) associationSoFar

        let newAssociation = (a, a') :| associationSoFar

        if length newAssociation == enoughMatches
            then pure newAssociation
            else go (toList newAssociation) xs' ys'

readCoord :: String -> Coord
readCoord str =
    let [x, y, z] = map read $ splitOn "," str
    in (x, y, z)

example :: ([Coord], [Coord])
example =
    ([(0 ,  2, 0), ( 4, 1, 0), ( 3,  3, 0)]
    ,[(-1, -1, 0), (-5, 0, 0), (-2,  1, 0)]
    )

minusCoord :: Coord -> Coord
minusCoord (x, y, z) = (-x, -y, -z)

coordAdd :: Coord -> Coord -> Coord
coordAdd (x, y, z) (x', y', z') = (x + x', y + y', z + z')

coordDiff :: Coord -> Coord -> Coord
coordDiff x y = coordAdd x (minusCoord y)

-- Around the origin
rotateX, rotateY, rotateZ :: Coord -> Coord

rotateX (x, y, z) = ( x, -z,  y)
rotateY (x, y, z) = ( z,  y, -x)
rotateZ (x, y, z) = ( y, -x,  z)

allRotations :: [Coord -> Coord]
allRotations =
    [ id
    , rotateZ
    , rotateZ . rotateZ
    , rotateZ . rotateZ . rotateZ

    , rotateY
    , rotateY . rotateZ
    , rotateY . rotateZ . rotateZ
    , rotateY . rotateZ . rotateZ . rotateZ

    , rotateY . rotateY
    , rotateY . rotateY . rotateZ
    , rotateY . rotateY . rotateZ . rotateZ
    , rotateY . rotateY . rotateZ . rotateZ . rotateZ

    , rotateY . rotateY . rotateY
    , rotateY . rotateY . rotateY . rotateZ
    , rotateY . rotateY . rotateY . rotateZ . rotateZ
    , rotateY . rotateY . rotateY . rotateZ . rotateZ . rotateZ

    , rotateX
    , rotateX . rotateZ
    , rotateX . rotateZ . rotateZ
    , rotateX . rotateZ . rotateZ . rotateZ

    , rotateX . rotateX . rotateX
    , rotateX . rotateX . rotateX . rotateZ
    , rotateX . rotateX . rotateX . rotateZ . rotateZ
    , rotateX . rotateX . rotateX . rotateZ . rotateZ . rotateZ
    ]

getTransformation :: NonEmpty (Coord, Coord)
                  -> Maybe (Coord -> Coord)
getTransformation xs =
    let left = fmap fst xs
        right = fmap snd xs

    in case mapMaybe (sequence . ((,) <*> slide . (`fmap` right))) allRotations of
        [(rotation, translation)] -> Just $ coordAdd translation . rotation
        _ -> Nothing
  where
    slide ys =
        let l = fst $ NE.head xs
            r = NE.head ys
            diff = coordDiff l r
        in if fmap fst xs == fmap (coordAdd diff) ys
            then Just diff
            else Nothing

combine :: ([(Coord , [Coord])], [[Coord]]) -> [[(Coord, [Coord])]]
combine (scanners, todo) = do
    -- traceM $ show scanners

    (scanner, rest) <- pick todo
    pairing <- maybeToList $ asum $
        map (match scanner . snd) scanners

    -- traceM $ show $ length scanners

    case getTransformation $ fmap swap pairing of
        Nothing -> error "No transformation worked! Oh, no!"
        Just t ->
          let transformed = fmap t scanner
          in if null rest
              then pure $ (t (0, 0, 0), transformed) : scanners
              else combine ((t (0, 0, 0), transformed) : scanners, rest)

coordAbsSum :: Coord -> Int
coordAbsSum (x, y, z) = abs x + abs y + abs z

manhattanDist :: Coord -> Coord -> Int
manhattanDist x y = coordAbsSum (coordDiff x y)

main :: IO ()
main = do
    input <- readFile "input/19.txt"
    let scanners = map (map readCoord . drop 1 . lines) $ splitOn "\n\n" input
        -- [s0, s1 , s2 , s3 , s4]  = scanners
        s0 = head scanners

    -- traverse_ print scanners
    let scannerPos = map fst $ head $ combine ([((0, 0, 0), s0)], drop 1 scanners)

    print $ maximum $ manhattanDist <$> scannerPos <*> scannerPos

    -- print $ length $ nub $ join solution
    -- print $ sum $ map length solution

    -- print $ match s0 s1
    {-
    print $ match s1 s4
    print $ match s4 s2
    print $ match s1 s3
    -}
