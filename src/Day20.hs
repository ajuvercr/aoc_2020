module Day20
    (solve
    ) where

import Lib
import NanoParsec
import Data.List
import Debug.Trace

data Tile = Tile
          { tile         :: Int
          , original     :: [String]
          , sides        :: [String]
          } deriving (Show)

data Square = Empty | T Tile deriving (Show)

-- safeHead :: [a] -> Maybe a
-- safeHead []    = Nothing
-- safeHead (x:_) = Just x


buildMap :: [Int] -> [Tile] -> Int -> Square -> Square -> Tile
buildMap done ts todo Empty Empty   = head (filter (filterCorner ts) ts)                               -- this only happens once
buildMap done ts todo Empty (T t1)  = head (filter (\x -> (tile x `notElem` done) && corner x && adjacentTile t1 x) ts)
    where corner x = edgeCount ts x > 1

buildMap done ts todo (T t1) Empty  = head (filter (\x -> (tile x `notElem` done) && corner x && adjacentTile t1 x) ts)
    where corner x = (todo /= 0 && edgeCount ts x == 3) || (todo == 0 && edgeCount ts x == 2)

buildMap done ts todo (T t1) (T t2) = head (filter (\x -> (tile x `notElem` done) && adjacentTile t1 x && adjacentTile t2 x) ts)


buildRow :: Int -> [Int] -> [Tile] -> [Square] -> ([Tile], [Int])
buildRow todo done ts sqs = finish $ foldr step ([], done, todo, Empty) sqs
    where
        step top (cum, done, todo, prev) = let n = buildMap done ts todo prev top in (n:cum, tile n:done, todo - 1, T n)
        finish (x, y, _, _) = (x, y)


buildRealMap :: [Tile] -> [[Tile]]
buildRealMap ts = finish $ iterate step ([], [], start) !! width
    where
        width = isqrt $ length ts
        start = replicate width Empty
        step (cum, done, prev) = let (row, done') = buildRow (width - 1) done ts prev in (row:cum, done', map T row)
        finish (x, _, _) = x


getSides :: [String] -> [String]
getSides tile = sides ++ map reverse sides
    where
        tile' = transpose tile
        sides = [head tile, last tile, head tile', last tile']


parseTile :: Parser Tile
parseTile = do
    reserved "Tile"
    n <- number
    reserved ":"
    tile <- star (plus (oneOf ".#") <* spaces)
    return $ Tile n tile (getSides tile)


adjacentTile :: Tile -> Tile -> Bool
adjacentTile Tile { tile=t1, sides=s1 } Tile { tile=t2, sides=s2 }
    | t1 == t2            = False
    | otherwise           = any (`elem` s2) s1


edgeCount :: [Tile] -> Tile -> Int
edgeCount ts t = count (adjacentTile t) ts


filterCorner :: [Tile] -> Tile -> Bool
filterCorner ts t = edgeCount ts t == 2


type Prep = [Tile]
prepare :: String -> Prep
prepare = runParser (star parseTile)


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print ((product . map tile) corners)
    where corners = filter (filterCorner x) x


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print (map (map tile) $ buildRealMap x)


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
