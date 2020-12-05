module Day01
    ( solve
    ) where

import Data.List
import Control.Applicative

parseinput :: String -> [Int]
parseinput content = map read $ lines content


solve1 :: Int -> [Int] -> [Int] -> Maybe Int
solve1 target (s:ss) (l:ll)
    | s + l < target = solve1 target ss (l:ll)
    | s + l > target = solve1 target (s:ss) ll
    | otherwise      = return $ s * l
solve1 _ _ _         = Nothing


firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f (x:xs) = f x <|> firstJust f xs
firstJust _ _      = Nothing


type Prep = ([Int], [Int])
prepare :: String -> Prep
prepare x = (sorted, reverse sorted)
    where sorted = sort . parseinput $ x


part1 :: Prep -> IO ()
part1 (sorted, revSorted) = putStr "Par 1: " >> print (solve1 2020 sorted revSorted)


part2 :: Prep -> IO ()
part2 (sorted, revSorted) = putStr "Part 2: " >> print (firstJust mapper revSorted)
    where mapper target = (* target) <$> solve1 (2020 - target) sorted revSorted


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
