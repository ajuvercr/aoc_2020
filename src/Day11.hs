{-# LANGUAGE TupleSections #-}

module Day11
    (solve
    ) where

import Lib
import Data.Maybe
import qualified Data.List
import Data.List.Ordered
import qualified Data.Map.Strict as M
-- import Data.Set (Set, empty, union, fromList, filter, difference, fromAscList)
import Control.Parallel.Strategies

type Coord = (Int, Int)
type AdjF = Coord -> [Coord]



(a1, a2) `plus` (b1, b2) = (a1+b1, a2+b2)


dirs :: [Coord]
dirs = [(1,0), (0,1), (-1,0), (0, -1), (1,1), (-1, -1), (1, -1), (-1,1)]


buildAdj :: AdjF -> [Coord] -> M.Map Coord [Coord]
buildAdj f cs = M.fromAscList $ map b cs
    where b x = (x, f x)


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


toMaybe True a = Just a
toMaybe False _ = Nothing


pmap f xs = map f xs `using` parList rdeepseq


sim :: Int -> [Coord] -> AdjF -> [Coord] -> [Coord]
sim thresh all adj occ = catMaybes $ pmap getFilter all  -- ENH: Can this go parallel?
    where
        isocc = has occ
        getFilter x
            | isocc x   = toMaybe (thresh > c x) x   -- Occ seat stays occ if thresh is bigger
            | otherwise = toMaybe (0 ==     c x) x   -- Emtpy seat changes if no adj seats are in occ
        c x = count isocc (adj x)


run :: Eq a => (a -> a) -> a -> a
run f a
    | a == a'   = a
    | otherwise = run f a'
    where a' = f a


type Prep = [Coord]
getcoords :: [String] -> [Coord]
getcoords x = Data.List.sort $ getcoords' 0 x
    where
        getcoords' _ [] = []
        getcoords' at (x:xs) = map (,at) (Data.List.elemIndices 'L' x) ++ getcoords' (at+1) xs


prepare :: String -> Prep
prepare = getcoords . lines

part1 :: Prep -> IO ()
part1 coord = putStr "Part 1: " >> print (length $ run r coord)
    where
        m = buildAdj adjacent coord
        r = sim 4 coord (m M.!)


part2 :: Prep -> IO ()
part2 coord = putStr "Part 2: " >> print (length $ run r coord)
    where
        m = buildAdj (adjacent' coord) coord
        r = sim 5 coord (m M.!)


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
