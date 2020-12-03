module Day03
    (solve
    ) where

data Slope = Slope {dx::Int, dy::Int} deriving (Show)


tree :: Char
tree = '#'

countTrees :: Slope -> [String] -> Int -> String
countTrees _ [] _ = []
countTrees s (x:xs) at = (cycle x !! at) : countTrees s (drop (dy s) xs) (at + dx s)

trySlope :: [String] -> Slope -> Int
trySlope tob slope = length $ filter (==tree) $ countTrees slope tob 0

solve :: String -> IO ()
solve x = do
    let tob = lines x

    putStr "Part 1: "
    print $ trySlope tob $ Slope 3 0
    putStr "Part 2: "
    print $ product $ map (trySlope tob) [Slope 1 0, Slope 3 0, Slope 5 0, Slope 7 0, Slope 1 1]
    return ()
