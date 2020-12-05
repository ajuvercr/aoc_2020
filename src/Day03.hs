module Day03
    (solve
    ) where

import Lib

data Slope = Slope {dx::Int, dy::Int} deriving (Show)

tree :: Char
tree = '#'

countTrees :: Slope -> [String] -> Int -> String
countTrees _ [] _ = []
countTrees s (x:xs) at = (cycle x !! at) : countTrees s (drop (dy s) xs) (at + dx s)

trySlope :: [String] -> Slope -> Int
trySlope tob slope = count (==tree) $ countTrees slope tob 0


type Prep = [String]
prepare :: String -> Prep
prepare = lines


part1 :: Prep -> IO ()
part1 tob = do
    putStr "Part 1: "
    print $ trySlope tob $ Slope 3 0


part2 :: Prep -> IO ()
part2 tob = do
    putStr "Part 2: "
    print $ product $ map (trySlope tob) [Slope 1 0, Slope 3 0, Slope 5 0, Slope 7 0, Slope 1 1]


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _ x = do
    let prep = prepare x
    part1 prep
    part2 prep
