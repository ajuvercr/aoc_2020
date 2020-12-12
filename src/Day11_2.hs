{-# LANGUAGE TupleSections #-}

module Day11_2
    (solve
    ) where

import DList
import Lib
import Data.Maybe
import qualified Data.List
import Data.List.Ordered
import qualified Data.Map.Strict as M
-- import Data.Set (Set, empty, union, fromList, filter, difference, fromAscList)
import Control.Parallel.Strategies

type Coord = (Int, Int)
-- type DDList a = DList (DList a)
type AdjF = Coord -> [Coord]

-- indexWith :: DDList Char -> Coord -> (Char, DDList Char)
-- indexWith l (x, y) = update' (`index` y) l x

(a1, a2) `plus` (b1, b2) = (a1+b1, a2+b2)


dirs :: [Coord]
dirs = [(1,0), (0,1), (-1,0), (0, -1), (1,1), (-1, -1), (1, -1), (-1,1)]


-- buildAdj :: AdjF -> DDList Char -> M.Map Coord [Coord]
-- buildAdj f cs = M.fromAscList $ map b cs
--     where b x = (x, f x)


adjacent :: AdjF
adjacent x = map (x `plus` ) dirs


adjacent' :: [Coord] -> AdjF
adjacent' all x = mapMaybe (foo all x) dirs
    where
        foo all x dx
            | nx `elem` all = Just nx
            | j > 100 || k > 100 || j < 0 || k < 0 = Nothing
            | otherwise     = foo all nx dx
            where nx@(j, k) = x `plus` dx




type Prep = [[Char]]

prepare :: String -> Prep
prepare = lines


part1 :: Prep -> IO ()
part1 coord = putStr "Part 1: "


part2 :: Prep -> IO ()
part2 coord = putStr "Part 2: "


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
