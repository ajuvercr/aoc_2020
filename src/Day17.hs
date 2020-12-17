module Day17
    (solve
    ) where

import Lib
import qualified Data.Map.Strict as M
import Data.MemoTrie ( memoFix )
import Debug.Trace

-- Inspired by https://www.47deg.com/blog/game-of-life-haskell/

type Point = [Int]
data Cell = Alive | Dead deriving(Eq, Show)
type Grid = Point -> Cell




getMin :: Ord a => [a] -> a
getMin [x] = x
getMin (x:xs) = if' (x<min) x min
  where min = getMin xs

gameOfLife :: Grid -> Integer -> Grid
gameOfLife initial l p = memoFix go (l, p)
  where
    go _r (0, p)
      = initial p
    go r (n, p) = nextStep (r (n-1, p))
                 (map (\x -> r (n-1, x)) (adjacents p))

    nextStep :: Cell -> [Cell] -> Cell
    nextStep Alive adj
      | count Alive adj == 2  = Alive  -- underpopulation
      | count Alive adj == 3  = Alive  -- overpopulation
      | otherwise             = Dead  -- live and let live
    nextStep Dead adj
      | count Alive adj == 3 = Alive  -- reproduction
      | otherwise            = Dead  -- nothing happens

    adjacents :: Point -> [Point]
    adjacents [x,y,z,w]
      = [[x+m, y+n, z+o, w+k] | m <- [-1,0,1], n <- [-1,0,1], o <- [-1,0,1], k <- [-1,0,1], (m,n,o,k) /= (0,0,0,0)]

    count :: Eq a => a -> [a] -> Int
    count x = length . filter (==x)

toCell :: Char -> Cell
toCell '.' = Dead
toCell '#' = Alive

fromCell :: Cell -> Char
fromCell Alive = '#'
fromCell Dead  = '.'

enumerate :: [a] -> [(Int, a)]
enumerate = enumerate' 0
    where
        enumerate' _ [] = []
        enumerate' i (x:xs) = (i, x):enumerate' (i+1) xs


parseGrid :: M.Map Point Cell -> (Int, String) -> M.Map Point Cell
parseGrid m (x, str) = foldl ins m (enumerate str)
    where ins m (y, c) = M.insert [y, x, 0, 0] (toCell c) m


dims :: ((Int, Int, Int), (Int, Int, Int))
dims = ((-6, -6, -6), (10, 10, 10))

-- printGame :: Grid -> [[String]]
-- printGame grid = [[[fromCell (grid [x,y,z]) | x <-[mwidth..width]] | y <- [mheight..height]] | z<-[mdepth..depth]]
--     where ((mwidth, mheight, mdepth), (width, height, depth)) = dims

countAlive :: Grid -> Int
countAlive grid = length $ filter (\x -> grid x==Alive) [[x,y,z,w] | x <-[mwidth..width], y <- [mheight..height], z<-[mdepth..depth], w<-[mdepth..depth]]
    where ((mwidth, mheight, mdepth), (width, height, depth)) = dims

type Prep = Grid
prepare :: String -> Prep
prepare x = lookup (trace (show map) map)
    where
        map = foldl parseGrid M.empty (enumerate $ lines x)
        lookup m x = M.findWithDefault Dead x m


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (countAlive s)
-- >> mapM_ (\x -> print "--------------------------------------------" >> mapM_ print x) (printGame s)
    where s = gameOfLife x 6


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: "


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
