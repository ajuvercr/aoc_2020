{-# LANGUAGE TupleSections #-}

module Day11
    (solve
    ) where

import Lib
import Data.Maybe
import qualified Data.List
import Data.List.Ordered
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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



sim :: Int -> [Coord] -> AdjF -> S.Set Coord -> S.Set Coord
sim thresh all adj occ = foldl step S.empty all  -- ENH: Can this go parallel?
    where
        occupAround coord = count (`S.member` occ) (adj coord)
        step m coord
            | not member && occupAround coord == 0     = coord `S.insert` m -- empty seat
            | member     && occupAround coord < thresh = coord `S.insert` m
            | otherwise                                = m
            where member = coord `S.member` occ


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
part1 coord = putStr "Part 1: " >> print (length $ run r S.empty)
    where
        m = buildAdj adjacent coord
        r = sim 4 coord (m M.!)


part2 :: Prep -> IO ()
part2 coord = putStr "Part 2: " >> print (length $ run r S.empty)
    where
        m = buildAdj (adjacent' coord) coord
        r = sim 5 coord (m M.!)


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
