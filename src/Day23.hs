{-# LANGUAGE BangPatterns #-}

module Day23
    (solve
    ) where

import Lib
import Data.Char
import Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as M

import Debug.Trace

type Map = M.IntMap Int

dec' :: Int -> Int -> Int
dec' n 1 = n
dec' _ x = x-1


getTarget :: (Int -> Int) -> (Int, Int, Int) -> Int -> Int
getTarget dec (x, y, z) c
  | x == c' || y == c' || z == c' = getTarget dec (x, y, z) c'
  | otherwise                     = c'
  where c' = dec c


step :: (Int -> Int) -> Map -> Int -> (Map, Int)
step dec m at = let x1 = m ! at
                    x2 = m ! x1
                    x3 = m ! x2
                    c  = m ! x3
                    t  = getTarget dec (x1, x2, x3) at
                    t' = m ! t
                in g' x1 x2 x3 c t t'
  where g' !x1 !x2 !x3 !c !t !t' = (M.insert at c $! M.insert x3 t' $! M.insert t x1 m, c)


buildMap :: [Int] -> Map
buildMap xs = M.fromList $ zip xs (tail xs ++ xs)


returnFromMap :: Int -> Map -> [Int]
returnFromMap s m = s: g s (m ! s)
  where
    g start at
      | start == at = []
      | otherwise   = at : g start (m ! at)


returnFromMap' :: Map -> [Int]
returnFromMap' = returnFromMap 1


type Prep = [Int]
prepare :: String -> Prep   -- 156794823
prepare = map digitToInt . filter isDigit


pretty :: [Int] -> String
pretty = concatMap show

times :: Int -> (a -> a) -> a -> a
times i f a
  | i == 0    = a
  | otherwise = times (i-1) f (f a)


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> putStrLn (pretty $ tail $ returnFromMap' map')
  where
    map = buildMap x
    dec = dec' (length x)
    step' = uncurry $ step dec
    start = (map, head x)
    (map', _) = times 100 step' start


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print (f1 * f2)
  where
    xs = x ++ [length x+1..1000000]
    map = buildMap xs
    dec = dec' 1000000
    step' = uncurry $ step dec
    start = (map, head x)
    (map', _) = times 10000000 step' start
    f1 = map' ! 1
    f2 = map' ! f1


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
