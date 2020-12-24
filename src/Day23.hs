module Day23
    (solve
    ) where

import Lib
import Data.Char
import Data.Bifunctor
import qualified Data.Map.Strict as M

import Debug.Trace

calcTarget :: Int -> [Int] -> Int
calcTarget 0 xs = calcTarget 9 xs
calcTarget t xs = if' (t `elem` xs) (calcTarget (t-1) xs) t

splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen f [] = ([],[])
splitWhen f (x:xs)
    | f x = ([], xs)
    | otherwise = first (x:) (splitWhen f xs)


fastTrace :: Show a => String -> a -> a
fastTrace t a = trace (t ++ show a) a

move :: [Int] -> [Int]
move (t:x:y:z:xs) = pre ++ (target:x:y:z:post) ++ [t]
    where
        target = calcTarget (t-1) [x,y,z]
        (pre, post) = splitWhen (==target) xs

type Prep = [Int]
prepare :: String -> Prep
prepare = map (read . (:[])) . filter isDigit


pretty :: [Int] -> String
pretty = concatMap show

part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (pretty (post ++ pre))
    where
        sol = iterate move x !! 100
        (pre, post) = splitWhen (==1) sol


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print (f1 * f2)
    where
        x' = x ++ [maximum x+1..1000000]
        sol = iterate move x' !! 10000000
        (_, f1:f2:_) = splitWhen (==1) sol


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
