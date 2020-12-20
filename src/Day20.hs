module Day20
    (solve
    ) where

import Lib
import NanoParsec
import Data.List
import Debug.Trace
import Control.Applicative

data Tile = Tile
          { tile         :: Int
          , original     :: [String]
          , sides        :: [String]
          } deriving (Show)

data Square = Empty | T Tile deriving (Show)

-- safeHead :: [a] -> Maybe a
-- safeHead []    = Nothing
-- safeHead (x:_) = Just x


-- ..................#.
-- #....##....##....###
-- .#..#..#..#..#..#...


filterSnake' :: [String] -> [String]
-- It matched!
filterSnake' ((s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9 :s10:s11:s12:s13:s14:s15:s16:s17:s18:'#':s20:xs)
            :('#':y2 :y3 :y4 :y5 :'#':'#':y8 :y9 :y10:y11:'#':'#':y14:y15:y16:y17:'#':'#':'#':ys)
            :(z1 :'#':z3 :z4 :'#':z6 :z7 :'#':z9 :z10:'#':z12:z13:'#':z15:z16:'#':z18:s19:z20:zs)
            :ks) =
         (s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9 :s10:s11:s12:s13:s14:s15:s16:s17:s18:'O':s20:xs')
        :('O':y2 :y3 :y4 :y5 :'O':'O':y8 :y9 :y10:y11:'O':'O':y14:y15:y16:y17:'O':'O':'O':ys')
        :(z1 :'O':z3 :z4 :'O':z6 :z7 :'O':z9 :z10:'O':z12:z13:'O':z15:z16:'O':z18:s19:z20:zs')
        :ks'
    where (xs':ys':zs':ks') = filterSnake' (xs:ys:zs:ks)

-- No match
filterSnake' ((x:xs):(y:ys):(z:zs):ks) = (x:xs'):(y:ys'):(z:zs'):ks'
    where (xs':ys':zs':ks') = filterSnake' (xs:ys:zs:ks)

filterSnake' ("":"":"":xs) = "":"":"":filterSnake' xs
filterSnake' x = x


filterSnake :: [String] -> [String]
filterSnake s1 = s4
    where
        s2 = filterSnake' s1
        s3 = filterSnake' (tail s2)
        s4 = head s2:head s3:filterSnake' (tail s3)


concat' (a, b) = a ++ b

middle :: [a] -> [a]
middle = tail . init

consMap :: [[[String]]] -> [String]
consMap [] = []
consMap (row:xs) = consMap xs ++ consRow
    where
        row' = map trueTile row
        trueTile x = map middle (middle x)

        consRow :: [String]
        consRow = foldl1 step row'

        step :: [String] -> [String] -> [String]
        step =  zipWith (flip (++))


rotl :: [[x]] -> [[x]]
rotl = transpose . map reverse


topLeft :: [String] -> (String, String)
topLeft x = (head x, (head . transpose) x)


rotateMatch :: (String -> Bool) -> (String -> Bool) -> [String] -> [String]
rotateMatch t l s | Just x <- r1 <|> r2 <|> r3 <|> r4 <|> t1 <|> t2 <|> t3 <|> t4 = x
    where
        st = reverse s
        r1 = let s' = iterate rotl s  !! 0; (top, left) = topLeft s'  in if' (t top && l left) (Just s') Nothing
        r2 = let s' = iterate rotl s  !! 1; (top, left) = topLeft s'  in if' (t top && l left) (Just s') Nothing
        r3 = let s' = iterate rotl s  !! 2; (top, left) = topLeft s'  in if' (t top && l left) (Just s') Nothing
        r4 = let s' = iterate rotl s  !! 3; (top, left) = topLeft s'  in if' (t top && l left) (Just s') Nothing

        t1 = let s' = iterate rotl st !! 0; (top, left) = topLeft s'  in if' (t top && l left) (Just s') Nothing
        t2 = let s' = iterate rotl st !! 1; (top, left) = topLeft s'  in if' (t top && l left) (Just s') Nothing
        t3 = let s' = iterate rotl st !! 2; (top, left) = topLeft s'  in if' (t top && l left) (Just s') Nothing
        t4 = let s' = iterate rotl st !! 3; (top, left) = topLeft s'  in if' (t top && l left) (Just s') Nothing


rotateSqaures :: [String] -> Maybe String -> Maybe String -> Tile -> [String]
rotateSqaures ts Nothing   Nothing   Tile{original=orig} = transpose $ rotateMatch (\x -> count (==x) ts == 1) (\x -> count (==x) ts == 1) orig
rotateSqaures ts (Just t1) Nothing   Tile{original=orig} = transpose $ rotateMatch (== t1) (\x -> count (==x) ts == 1) orig
rotateSqaures ts Nothing   (Just t2) Tile{original=orig} = transpose $ rotateMatch (\x -> count (==x) ts == 1) (== t2) orig
rotateSqaures ts (Just t1) (Just t2) Tile{original=orig} = transpose $ rotateMatch (== t1) (== t2) orig


rotateRow :: [String] -> [Maybe String] -> [Tile] -> [[String]]
rotateRow ts tops tiles = fst $ foldl step ([], Nothing) (zip tops tiles)
    where
        step (acc, prev) (top, tile) = let s = rotateSqaures ts prev top tile in (s:acc, Just $ map last s)


rotateMap :: [String] -> [[Tile]] -> [[[String]]]
rotateMap ts tiles = fst $ foldl step ([], start) tiles
    where
        start = map (const Nothing) (head tiles)
        step (cum, top) tiles = let row = rotateRow ts top tiles in (row:cum, reverse $ map (Just . last) row)


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
part2 x = putStr "Part 2: " >> print (sum $ map (count (=='#')) snaked)
    where
        world = reverse $ map reverse (buildRealMap x)
        edges = concatMap sides x
        rotated = rotateMap edges world
        consed = rotl $ rotl $ rotl $ consMap rotated
        snaked = filterSnake consed


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
