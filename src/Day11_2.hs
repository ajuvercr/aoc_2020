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
import Debug.Trace
type Coord = (Int, Int)
-- type DDList a = DList (DList a)
type AdjF = Coord -> [Coord]

-- indexWith :: DDList Char -> Coord -> (Char, DDList Char)
-- indexWith l (x, y) = update' (`index` y) l x

(a1, a2) `plus` (b1, b2) = (a1+b1, a2+b2)


dirs :: [Coord]
dirs = [(1,0), (0,1), (-1,0), (0, -1), (1,1), (-1, -1), (1, -1), (-1,1)]


adjacent :: AdjF
adjacent x = map (x `plus` ) dirs

-- buildAdj :: AdjF -> DDList Char -> M.Map Coord [Coord]
-- buildAdj f cs = M.fromAscList $ map b cs
--     where b x = (x, f x)

enum :: [a] -> [(Int, a)]
enum = enum' 0
    where
        enum' _ [] = []
        enum' i (x:xs) = (i, x):enum' (i+1) xs

run :: Eq a => (a -> a) -> a -> a
run f a
    | a == a'   = a
    | otherwise = run f a'
    where a' = f a


applyRules :: AdjF -> Coord -> Char -> DList (DListState Char) Char
applyRules _ _ '.' = return '.'
applyRules adj coord t = do
    neigs <- map2d' $ adj coord
    let c = count (=='R') neigs
    let b = if' (t == 'L') (c==0) (c<4)
    return $ if' b 'R' 'L'


applyRowRules :: AdjF -> Int -> Int -> DList (DListState Char) [Char]
applyRowRules adj y xmas = foldWith' u (:) [] coords
    where
        coords = map (y,) (reverse [0..xmas+1])
        u coord = do
            c <- get2d' coord
            applyRules adj coord c

sim :: Int -> Int -> AdjF -> [[Char]] -> [[Char]]
sim xmas ymas f bord = fst $ runDList2d foo bord
    where
        row i = applyRowRules f i xmas
        foo = foldWith' row (:) [] (reverse [0..ymas+1])


-- adjacent' :: [Coord] -> AdjF
-- adjacent' all x = mapMaybe (foo all x) dirs
--     where
--         foo all x dx
--             | nx `elem` all = Just nx
--             | j > 100 || k > 100 || j < 0 || k < 0 = Nothing
--             | otherwise     = foo all nx dx
--             where nx@(j, k) = x `plus` dx


printbord :: [[Char]] -> IO ()
printbord = printArray


type Prep = ([[Char]], Int, Int)

prepare :: String -> Prep
prepare x = let b = lines x
                (xmax, ymax) = (length b, length $ head b)
            in ([empty] ++ (map addempt . lines) x ++ [empty], xmax, ymax)
    where
        addempt line = ['.'] ++ line ++ ['.']
        empty = map (const '.') [0..100]


countResult :: [[Char]] -> Int
countResult bord = sum $ map (count (=='R')) bord

part1 :: Prep -> IO ()
part1 (bord, ymax, xmax) =
    putStr "Part 1: " >> print (countResult (run (sim xmax ymax adjacent) bord))


part2 :: Prep -> IO ()
part2 coord = putStr "Part 2: "


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
