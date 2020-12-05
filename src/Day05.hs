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


solve :: String -> IO ()
solve x = do
    putStr "Part 1: "
    let ids = map seatid $ parseSeats x
    print $ foldl max 0 ids

    putStr "Part 2: "
    print $ findgap $ sort ids
