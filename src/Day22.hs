module Day22
    (solve
    ) where

import Lib
import NanoParsec
import qualified Data.Set as S


finish :: [Int] -> Int
finish x = sum (zipWith (*) [1..] $ reverse x)


parsePlayer :: Parser [Int]
parsePlayer = do
    reserved "Player " >> number >> reserved ":"
    star (s2 number)


type Prep = ([Int], [Int])
prepare :: String -> Prep
prepare = runParser $ (,) <$> parsePlayer <*> parsePlayer


play1 :: ([Int], [Int]) -> [Int]
play1 (x, [])      = x
play1 ([], y)      = y
play1 (x:xs, y:ys)
    | x > y = play1 (xs ++ [x, y], ys)
    | y > x = play1 (xs,           ys ++ [y, x])


-- True = Player 1 wins, False == Player 2 wins
subGame :: ([Int], [Int]) -> Bool
subGame x = snd $ play2 S.empty x


play2 :: S.Set ([Int], [Int]) -> ([Int], [Int]) -> ([Int], Bool)
play2 dones (x, [])      = (x, True)
play2 dones ([], y)      = (y, False)
play2 dones (xs, ys) | S.member (tail xs, tail ys) dones = (xs, True)
play2 dones (x:xs, y:ys) = play2 newstate $
    if' n
        (if' (x > y) (nxs, ys) (xs , nys))
        (if' (subGame (take x xs, take y ys)) (nxs, ys) (xs , nys))
    where
        n = x > length xs || y > length ys
        nxs = xs ++ [x, y]
        nys = ys ++ [y, x]
        newstate = S.insert (xs, ys) dones


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (finish played)
    where played = play1 x


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print (finish $ fst played)
    where played = play2 S.empty x


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
