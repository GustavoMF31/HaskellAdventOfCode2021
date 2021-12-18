import Numeric.Natural (Natural)
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
type Nat = Natural

-- SnNumber :: *
-- SnInt :: Int -> SnNumber
-- SnPair :: SnNumber -> SnNumber -> SnNumber
data SnNumber =
    SnInt Int
  | SnPair SnNumber SnNumber
  deriving Eq

(f .: g) a b = f (g a b)

add :: SnNumber -> SnNumber -> SnNumber
add = simplify .: SnPair

addLeft :: Int -> SnNumber -> SnNumber
addLeft x (SnInt n) = SnInt $ x + n
addLeft x (SnPair sn sn') = SnPair (addLeft x sn) sn'

addRight :: Int -> SnNumber -> SnNumber
addRight x (SnInt n) = SnInt $ x + n
addRight x (SnPair sn sn') = SnPair sn (addRight x sn')

simplifyExplode :: SnNumber -> SnNumber
simplifyExplode s = maybe s fst (go 0 s)
  where
    go :: Nat -> SnNumber -> Maybe (SnNumber, (Int, Int))
    go depth (SnInt n) = Nothing
    go depth p@(SnPair (SnInt x) (SnInt y))
        | depth == 4 = Just (SnInt 0, (x, y))
    go depth (SnPair snl snr) = case go (depth + 1) snl of
        Just (snl', (addLL, addLR)) ->
            Just (SnPair snl' (addLeft addLR snr), (addLL, 0))
        Nothing -> case go (depth + 1) snr of
          Nothing -> Nothing
          Just (snr', (addRL, addRR)) ->
              Just (SnPair (addRight addRL snl) snr', (0, addRR))

{- 
    go :: Nat -> SnNumber -> (SnNumber, (Int, Int))
    go depth (SnInt n) = (SnInt n, (0, 0))
    go depth p@(SnPair (SnInt x) (SnInt y))
        | depth == 4 = (SnInt 0, (x, y))
    go depth (SnPair snl snr) = case go (depth + 1) snl of
        (snl', (addLL, addLR)) -> case go (depth + 1) (addLeft addLR snr) of
            (snr', (addRL, addRR)) ->
                (SnPair (addRight addRL snl') snr', (addLL, addRR))

    case (go (depth + 1) snl, go (depth + 1) snr) of
         ((snl', (addLL, addLR)), (snr', (addRL, addRR))) ->
            (SnPair (addRight addRL snl') (addLeft addLR snr'), (addLL, addRR))
    -}

simplifySplit :: SnNumber -> SnNumber
simplifySplit s = fromMaybe s $ go s
  where
    go :: SnNumber -> Maybe SnNumber
    go (SnInt n)
        | n > 9     = Just $ SnPair (SnInt $ n `div` 2) (SnInt $ ceiling (fromIntegral n / 2))
        | otherwise = Nothing
    go (SnPair snl snr) = case go snl of
        Just snl' -> Just $ SnPair snl' snr
        Nothing   -> SnPair snl <$> go snr

simplify :: SnNumber -> SnNumber
simplify sn =
    let simplified = simplifyExplode sn
    in if sn == simplified
        then let simplified' = simplifySplit simplified
             in if simplified' == simplified
                then simplified'
                else simplify simplified'
        else simplify simplified

example :: SnNumber
example = SnPair (SnPair (SnPair (SnPair (SnPair (SnInt 9) (SnInt 8)) (SnInt 1)) (SnInt 2)) (SnInt 3)) (SnInt 4)

parse :: String -> (SnNumber, String)
parse n = if not . null $ takeWhile isDigit n
    then let d = takeWhile isDigit n
         in (SnInt $ read d, drop (length d) n)
    else let (snl, rest ) = parse (drop 1 n)
             (snr, rest') = parse (drop 1 rest)
        in (SnPair snl snr, drop 1 rest')

display :: SnNumber -> String
display (SnInt n) = show n
display (SnPair l r) = "[" ++ display l ++ "," ++ display r ++ "]"

magnitude :: SnNumber -> Int
magnitude (SnInt n) = n
magnitude (SnPair sn sn') =
    3 * magnitude sn + 2 * magnitude sn'

main :: IO ()
main = do
    input <- readFile "input/18.txt"
    let sns = map (fst . parse) $ lines input
        -- final = foldl1 add sns

    -- print $ magnitude final
    -- traverse_ (print . display) final
    print $ maximum $ map (magnitude . uncurry add) $
        concatMap (\x -> [x, swap x]) $ (,) <$> sns <*> sns
