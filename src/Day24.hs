{-# LANGUAGE TupleSections #-}

module Day24
    (solve
    ) where

import Lib
import NanoParsec
import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Set as S


data Move = E | SE | SW | W | NW | NE deriving (Show)
type Coord = (Int, Int)

(.+.) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(a1, a2) .+. (b1, b2) = (a1+b1, a2+b2)

neighbours :: Coord -> [Coord]
neighbours x = map (x .+.) [(2, 0), (-2, 0), (1, 1), (-1, 1), (1, -1), (-1, -1)]


validCoord :: Coord -> Bool
validCoord (x, y) = x `mod` 2 == y `mod` 2


bounds :: Int
bounds = 150


field :: [Coord]
field = [(i, j) | i <- [-bounds..bounds], j <- [-bounds..bounds], validCoord (i, j)]


parseMove :: Parser Move
parseMove =   E  <$ string "e"
          <|> SE <$ string "se"
          <|> SW <$ string "sw"
          <|> W  <$ string "w"
          <|> NW <$ string "nw"
          <|> NE <$ string "ne"


applyMove :: Coord -> Move -> Coord
applyMove (x, y) E = (x+2, y)
applyMove (x, y) W = (x-2, y)
applyMove (x, y) NE = (x+1, y+1)
applyMove (x, y) NW = (x-1, y+1)
applyMove (x, y) SE = (x+1, y-1)
applyMove (x, y) SW = (x-1, y-1)

movesTo :: [Move] -> Coord
movesTo = foldl applyMove (0,0)


buildMap :: [[Move]] -> M.Map Coord Bool
buildMap x = M.fromListWith (/=) (map ((,True) . movesTo) x)

ll :: Prep -> Coord -> Bool
ll m c = S.member c m


isBlack :: Prep -> Coord -> Bool
isBlack x c
    | ll x c    = adjBlacks == 1 || adjBlacks == 2
    | otherwise = adjBlacks == 2
    where adjBlacks = count (ll x) (neighbours c)


consideringTiles :: Prep -> Prep
consideringTiles x = S.unions $ S.map (S.fromList . neighbours) x


gameOfLife :: Prep -> Prep
gameOfLife x = S.filter (isBlack x) (consideringTiles x)


type Prep = S.Set Coord
prepare :: String -> Prep
prepare x = M.foldlWithKey step S.empty $ buildMap $ map (runParser (star parseMove)) $ lines x
    where
        step s c True  = S.insert c s
        step s _ False = s


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (S.size x)


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print (S.size $ iterate gameOfLife x !! 100)


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
