module Day17
    (solve
    ) where

import Lib
import qualified Data.Map.Strict as M
import Data.MemoTrie ( memoFix )
import Debug.Trace

-- Inspired by https://www.47deg.com/blog/game-of-life-haskell/

newtype Point3 = P3 (Int, Int, Int) deriving(Eq, Ord, Show)
newtype Point4 = P4 (Int, Int, Int, Int) deriving(Eq, Ord, Show)

type Point = [Int]

data Cell = Alive | Dead deriving(Eq, Show)

type Grid a = M.Map a Cell


class Ord a => Coord a where
  neighbours              :: a -> [a]
  inbounds                :: Int -> a -> Bool
  purify                  :: a -> a
  shouldPurify            :: a -> Bool
  worth                   :: a -> Int


instance Coord Point3 where
  neighbours (P3 (x, y, z))     = [P3 (x+m, y+n, z+o) | m <- [-1,0,1], n <- [-1,0,1], o <- [-1,0,1], (m,n,o) /= (0,0,0)]
  inbounds level (P3 (x, y, z)) = (level * 3) > abs x + abs y + abs z - 16
  purify (P3 (a, b, c))         = P3 (a, b, abs c)
  shouldPurify (P3 (_, _, a))   = a < 0
  worth (P3 (_, _, a))          = if' (a>0) 2 1


point3All :: [Point3]
point3All = [P3 (x, y, z) | x <-[-6..10], y <- [-6..10], z<-[-6..10]]


point4All :: [Point4]
point4All = [P4 (x, y, z, w) | x <-[-6..10], y <- [-6..10], z<-[-6..10], w<-[-6..10]]


instance Coord Point4 where
  neighbours (P4 (x, y, z, w))     = [P4 (x+m, y+n, z+o, w+k) | m <- [-1,0,1], n <- [-1,0,1], o <- [-1,0,1], k <- [-1,0,1], (m,n,o,k) /= (0,0,0,0)]
  inbounds level (P4 (x, y, z, w)) = (level * 4) > abs x + abs y + abs z + abs w - 16
  purify (P4 (a, b, c, d))         = P4 (a, b, abs c, abs d)
  shouldPurify (P4 (_, _, a, d))   = a < 0 || d < 0
  worth (P4 (_, _, a, b))
    | a > 0 && b > 0 = 4
    | a > 0 || b > 0 = 2
    | otherwise      = 1


getMin :: Ord a => [a] -> a
getMin [x] = x
getMin (x:xs) = if' (x<min) x min
  where min = getMin xs


get :: Coord a => M.Map a Cell -> a -> Cell
get m a = M.findWithDefault Dead (purify a) m


gameOfLife :: Coord a => [a] -> Grid a -> Int -> Grid a
gameOfLife todo orig level = foldl step M.empty todo
  where
    aliveAround coord = count (==Alive) (map (get orig) (neighbours coord))
    step m coord
      | shouldPurify coord = m
      | not (inbounds level coord) = m
      -- Actually do something
      | get orig coord == Alive && aliveAround coord == 2 = M.insert coord Alive m
      | aliveAround coord == 3 = M.insert coord Alive m
      | otherwise              = m


enumerate :: [a] -> [(Int, a)]
enumerate = enumerate' 0
    where
        enumerate' _ [] = []
        enumerate' i (x:xs) = (i, x):enumerate' (i+1) xs


parseGrid :: [(Int, Int)] -> (Int, String) -> [(Int, Int)]
parseGrid m (x, str) = foldl ins m (enumerate str)
    where
      ins m (y, c)
        | c == '#'  = (x, y):m
        | otherwise = m


dims :: ((Int, Int, Int), (Int, Int, Int))
dims = ((-6, -6, -6), (10, 10, 10))


countAlive :: Coord a => Grid a -> Int
countAlive grid = sum (map getWorth (M.assocs grid))
  where
    getWorth (c, _)    = worth c


type Prep = [(Int, Int)]
prepare :: String -> Prep
prepare x = foldl parseGrid [] (enumerate $ lines x)


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (countAlive (foldl step grid [1..6]))
  where
    step = gameOfLife point3All
    grid = M.fromList (map (\(x,y) -> (P3 (x, y, 0), Alive)) x)


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print (countAlive (foldl step grid [1..6]))
  where
    step = gameOfLife point4All
    grid = M.fromList (map (\(x,y) -> (P4 (x, y, 0, 0), Alive)) x)


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
