module Day05
    (solve
    ) where

import Data.List
import NanoParsec

data Seat = Seat {x::Int, y::Int} deriving (Show)


parseSeats :: String -> [Seat]
parseSeats = map (runParser parseSeat) . lines


parseSeat :: Parser Seat
parseSeat = do
    row <- bin 64 'B' <$> parseTimes 7 item
    col <- bin 4 'R' <$> parseTimes 3 item
    spaces
    return $ Seat row col


bin :: Int -> Char -> String -> Int
bin _ _ [] = 0
bin s m (x:xs)
    | m == x = s + bin (s `div` 2) m xs
    | otherwise = bin (s `div` 2) m xs


seatid :: Seat -> Int
seatid (Seat x y) = x * 8 + y


findgap :: [Int] -> Int
findgap (x:y:xs)
    | x + 1 == y = findgap (y:xs)
    | otherwise  = x + 1


type Prep = [Int]
prepare :: String -> Prep
prepare = map seatid . parseSeats

part1 :: Prep -> IO ()
part1 x = do
    putStr "Par 1: "
    print $ foldl max 0 x


part2 :: Prep -> IO ()
part2 x = do
    putStr "Par 2: "
    print $ findgap $ sort x


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _ x = do
    let prep = prepare x
    part1 prep
    part2 prep
