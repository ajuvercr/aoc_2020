{-# LANGUAGE TupleSections #-}

module Day11
    (solve
    ) where

import Lib
import Data.Maybe
import qualified Data.List
import Data.Set (Set, empty, union, fromList, filter, difference)

type Coord = (Int, Int)
type FilterF = Set Coord -> Coord -> Bool
type AdjF = Coord -> [Coord]

(a1, a2) `plus` (b1, b2) = (a1+b1, a2+b2)


dirs :: [Coord]
dirs = [(1,0), (0,1), (-1,0), (0, -1), (1,1), (-1, -1), (1, -1), (-1,1)]


adjacent :: AdjF
adjacent x = map (x `plus` ) dirs


adjacent' :: Set Coord -> AdjF
adjacent' all x = mapMaybe (foo all x) dirs
    where
        foo all x dx
            | nx `elem` all = Just nx
            | j > 100 || k > 100 || j < 0 || k < 0 = Nothing
            | otherwise     = foo all nx dx
            where nx@(j, k) = x `plus` dx


staysempty :: AdjF -> FilterF
staysempty adj xs x = any (`elem` xs) (adj x)


staysfull :: Int -> AdjF -> FilterF
staysfull treshhold adj xs x = treshhold > count (`elem` xs) (adj x)


simulate :: FilterF -> FilterF -> Set Coord -> (Set Coord, Set Coord) -> (Set Coord, Set Coord)
simulate e f all (vacent, occup)= (va, all `difference` va)
    where
        va1 = Data.Set.filter (e occup) vacent
        va2 = Data.Set.filter (not . f occup) occup
        va = va1 `union` va2


simulate' :: FilterF -> FilterF -> Set Coord -> (Set Coord, Set Coord) -> Int
simulate' e f all x
    | x == x' = length $ snd x
    | otherwise = simulate' e f all x'
    where x' = simulate e f all x


sim :: Int -> Set Coord -> AdjF -> Set Coord -> Set Coord
sim thresh all adj occ = Data.Set.filter getFilter all
    where
        getFilter x
            | x `elem` occ = c (thresh >) (adj x)   -- Occ seat stays occ if thresh is bigger
            | otherwise    = c (0 ==)     (adj x)   -- Emtpy seat changes if no adj seats are in occ
        c f = f . count (`elem` occ)

run :: Eq a => (a -> a) -> a -> a
run f a
    | a == a' = a
    | otherwise = run f a'
    where a' = f a



type Prep = Set Coord
getcoords :: [String] -> Set Coord
getcoords x = fromList $ getcoords' 0 x
    where
        getcoords' _ [] = []
        getcoords' at (x:xs) = map (,at) (Data.List.elemIndices 'L' x) ++ getcoords' (at+1) xs


prepare :: String -> Prep
prepare = getcoords . lines

part1 :: Prep -> IO ()
part1 coord = putStr "Part 1: " >> print (length $ run r coord)
    where r = sim 4 coord adjacent


part2 :: Prep -> IO ()
part2 coord = putStr "Part 2: " >> print (length $ run r coord)
    where r = sim 5 coord (adjacent' coord)
-- part2 coords = putStr "Part 2: " >> print (simulate' e f coords (coords, empty))
--     where
--         adj = adjacent' coords
--         e = staysempty adj
--         f = staysfull 5 adj


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
