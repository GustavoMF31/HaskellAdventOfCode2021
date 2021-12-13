{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Char (isUpper)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Control.Monad (guard, when)
import Data.List (genericLength)

infixr .:
(.:) = (.) . (.)
count = genericLength .: filter . (==)

type CaveMap = [(String, String)]
canBeVisitedTwice = all isUpper

adjacent :: CaveMap -> String -> [String]
adjacent caves x = mapMaybe getAdjacent caves
  where
    getAdjacent :: (String, String) -> Maybe String
    getAdjacent (l, r)
        | x == l = Just r
        | x == r = Just l
        | otherwise = Nothing

paths :: CaveMap -> [[String]]
paths caves = map reverse $ go False "start" []
  where
    go :: Bool -> String -> [String] -> [[String]]
    go visitedSmallTwice c pathBefore = do
      let currentPath = c : pathBefore
      nextCave <- filter (/= "start") $ adjacent caves c

      let canBeRegularVisit = canBeVisitedTwice nextCave
            || count nextCave currentPath == 0
          secondSmallVisit = not canBeRegularVisit
            && not visitedSmallTwice

      guard $ canBeRegularVisit || secondSmallVisit

      if nextCave == "end"
        then pure (nextCave : currentPath)
        else go (secondSmallVisit || visitedSmallTwice)
                nextCave currentPath

parse :: String -> CaveMap
parse = map (ensurePair . splitOn "-") . lines
  where
    ensurePair [a, b] = (a, b)
    ensurePair _ = error "Bad input"

main = do
    input <- readFile "input/12.txt"
    print $ length $ paths $ parse input
    -- traverse print $ paths $ parse input
    -- print $ adjacent (parse input) "start"
