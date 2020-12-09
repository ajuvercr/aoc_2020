module Day09
    (solve
    ) where

import Lib
import Debug.Trace

preamble = 25

max' :: [Int] -> Int
max' = foldl max 0

min' :: [Int] -> Int
min' = foldl min 10000000000000

type Prep = ([Int], [Int])
prepare :: String -> Prep
prepare x = let ns = map read $ lines x in
    (take preamble ns, drop preamble ns)

isvalid :: [Int] -> Int -> Bool
isvalid (x:xs) i
    | x >= i = isvalid xs i
    | otherwise = part2 || isvalid xs i
    where part2 = (x + x /= i && (i - x) `elem` xs)
isvalid [] _ = False

dopart1 :: [Int] -> [Int] -> Int
dopart1 p (x:xs)
    | isvalid p x = dopart1 (tail p ++ [x]) xs
    | otherwise   = x

dopart2 :: [Int] -> [Int] -> Int -> [Int]
dopart2 [] _ _ = [42]
dopart2 (x:xs) s target
    | summed == target = s
    | summed >  target = dopart2 (x:xs) (tail s) target
    | otherwise        = dopart2 xs (s ++ [x]) target
    where summed = sum s


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (uncurry dopart1 x)


part2 :: Prep -> IO ()
part2 (p, r) = putStr "Part 2: " >> print (min' p2 + max' p2)
    where p2 = dopart2 (p ++ r) [] 217430975


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
