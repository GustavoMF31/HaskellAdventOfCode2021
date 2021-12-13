import Data.List.Split (splitOn)
import Data.List (genericLength, intersect, partition, find)
import Numeric.Natural (Natural)
import Control.Monad (join)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Bifunctor (first)
type Nat = Natural

(.:) = (.) . (.)
count = genericLength .: filter

parse :: String -> [([String], [String])]
parse str = map ((\[a, b] -> (words a, words b)) . splitOn "|") $ lines str

solve1 :: [([String], [String])] -> Nat
solve1 = count ((`elem` [2, 4, 3, 7]) . length) . concatMap snd

getSingleton :: [a] -> Maybe a
getSingleton [x] = Just x
getSingleton _ = Nothing

safeHead :: [a] -> Maybe a
safeHead (x : xs) = Just x
safeHead _ = Nothing

note :: String -> Maybe a -> Either String a
note s Nothing = Left s
note _ (Just a) = Right a

solveLine :: [String] -> Either String [(String, Nat)]
solveLine input = do
    one   <- note "No str of length two"   $ getSingleton $ filter ((== 2) . length) input
    four  <- note "No str of length four"  $ getSingleton $ filter ((== 4) . length) input
    seven <- note "No str of length three" $ getSingleton $ filter ((== 3) . length) input
    eight <- note "No str of length seven" $ getSingleton $ filter ((== 7) . length) input

    let rest = filter (`notElem` [one, four, seven, eight]) input
    
    let topAndBottom = foldl1 intersect rest
        -- restWithoutTopAndBottom = map (filter (`notElem` topAndBottom)) rest
        (length5, length6) = partition ((== 5) . length) rest

    three <- note "Couldn't get three" $ getSingleton $ filter ((S.fromList one `S.isSubsetOf`) . S.fromList) length5
    let twoAndFive = filter (/= three) length5
    nine <- note "Couldn't get nine" $ getSingleton $ filter ((S.fromList three `S.isSubsetOf`) . S.fromList) length6
    let zeroAndSix = filter (/= nine) length6
    middle <- note "Failed to get middle" $ getSingleton $ foldl1 intersect $ map (filter (`notElem` topAndBottom)) twoAndFive
    six <- note "Failed to get six" $ getSingleton $ filter (middle `elem`) zeroAndSix
    zero <- note "Failed to get zero" $ getSingleton $ filter (/= six) zeroAndSix
    five <- note "Failed to get five" $ getSingleton $ filter ((== 3) . length . intersect four) twoAndFive
    two <- note "Failed to get two" $ getSingleton $ filter (/= five) twoAndFive
    pure
      [ (zero, 0)
      , (one, 1)
      , (two, 2)
      , (three, 3)
      , (four, 4)
      , (five, 5)
      , (six, 6)
      , (seven, 7)
      , (eight, 8)
      , (nine, 9)
      ]

lookupCode :: (Ord a, Eq a) => [a] -> [([a], b)] -> Maybe b
lookupCode x =
    fmap snd . find ((== S.fromList x) . S.fromList . fst)

getLineCode :: ([String], [String]) -> Either String Nat
getLineCode (segments, code) =
    let result = solveLine segments
    in case result of
        Right mapping ->
            let nats = map (`lookupCode` mapping) code
            in case sequence nats of 
                Nothing ->  Left "Failed a lookup"
                Just digits -> Right $ sum $
                    zipWith (*) [1000, 100, 10, 1] digits 
        Left e -> Left e

main :: IO ()
main = do
    input <- readFile "input/8.txt"

    let xs = parse input
    case traverse getLineCode xs of
        Left e -> putStrLn e
        Right nats -> print $ sum nats
    {-
    print $ getLineCode $ head xs
    print $ solveLine $ fst $ head xs
    print $ snd $ head xs
    -}

-- febcgd  = 0
-- bd      = 1
-- feacd = 2
-- feadb   = 3
-- dagb    = 4
-- feagb = 5
-- feagbc = 6
-- dbf     = 7
-- dgcaefb = 8
-- feagbd = 9

-- {f, e}  ∈ {top, bottom}
-- c = bottom-left
-- a = middle
-- d = top-right

{-
febcgd = 0
bd = 1
feacd = 2
feadb = 3
dagb = 4
feagb = 5
feabcg = 6
dbf = 7
dgcaefb = 8
febagd = 9
-}
