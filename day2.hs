import Data.Bifunctor (first, second)

data Instruction = Up | Down | Forward
    deriving (Show)

execute :: Instruction -> Int -> (Int, Int) -> (Int, Int)
execute Up      = second . (+) . negate
execute Down    = second . (+)
execute Forward = first  . (+)

execute2 :: Instruction -> Int -> (Int, Int, Int) -> (Int, Int, Int)
execute2 Up      n (x, y, aim) = (x, y, aim - n)
execute2 Down    n (x, y, aim) = (x, y, aim + n)
execute2 Forward n (x, y, aim) = (x + n, y + n * aim, aim)

-- Parsing is built with unsafe functions
parseInstruction :: String -> Instruction
parseInstruction "up" = Up
parseInstruction "down" = Down
parseInstruction "forward" = Forward
parseInstruction inst = error $ "Bad instruction: " ++ inst

parseLine :: String -> (Instruction, Int)
parseLine str = let [inst, dist] = words str in (parseInstruction inst, read dist)

solve :: [(Instruction, Int)] -> Int
solve xs =
    let (x, y, _) = foldl (flip $ uncurry execute2) (0, 0, 0) xs
    in x * y

main :: IO ()
main = do
    input <- readFile "input/2.txt"
    print $ solve $ map parseLine $ lines input
