{-# LANGUAGE TypeApplications #-}
import Data.List.NonEmpty (NonEmpty ((:|)), cons)
import qualified Data.List.NonEmpty as NE
import Numeric.Natural (Natural)
import Data.List (sort)
import Data.Either (rights)
type Nat = Natural

data BracketType = Paren | Square | Curly | Angle deriving (Eq, Show)
data Bracket = Opening BracketType | Closing BracketType deriving (Show)

{-
check :: [Bracket] -> Either (NonEmpty BracketType, [Bracket]) ()
check [] = Right ()
check (Opening b : xs) = case check xs of
    Right () -> Right () -- Unfinished line - Opening bracket never closed
    Left (l@(b' :| bs), xs') -> if b == b'
        then case NE.nonEmpty bs of
            Nothing -> check xs'
            Just neXs -> Left (neXs, xs')
        else Left (cons b l, xs')
check (Closing b : xs) = Left (pure b, xs)
-}

check' :: [Bracket] -> Either BracketType [BracketType]
check' = go []
  where
    go :: [BracketType] -> [Bracket] -> Either BracketType [BracketType]
    go stack [] = Right stack
    go stack (Opening b : bs) = go (b : stack) bs
    go [] (Closing b : bs) = Left b
    go (x : xs) (Closing b : bs) = if x == b then go xs bs else Left b

score :: BracketType -> Nat
score Paren = 3
score Square = 57
score Curly = 1197
score Angle = 25137

score2 :: BracketType -> Nat
score2 Paren = 1
score2 Square = 2
score2 Curly = 3
score2 Angle = 4

scoreCompletion :: [BracketType] -> Nat
scoreCompletion = foldl (\x b -> score2 b + 5 * x) 0

parseBracket :: Char -> Bracket
parseBracket '(' = Opening Paren
parseBracket '[' = Opening Square
parseBracket '{' = Opening Curly
parseBracket '<' = Opening Angle
parseBracket ')' = Closing Paren
parseBracket ']' = Closing Square
parseBracket '}' = Closing Curly
parseBracket '>' = Closing Angle
parseBracket _ = error "Bad bracket"

-- unsafe
middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

main :: IO ()
main = do
    input <- readFile "input/10.txt"
    let bracketLines = map (map parseBracket) $ lines input
    -- print $ check $ head bracketLines
    -- print $ sum $ map (either (score . NE.last . fst) (const 0) . check) bracketLines
    -- print $ sum $ map (either score (const 0) . check') bracketLines
    print @Nat $ middle $ sort $ map scoreCompletion $ rights
        $ map check' bracketLines
