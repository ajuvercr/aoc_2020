module Day22
    (solve
    ) where

import Lib
import NanoParsec
import Debug.Trace
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
    | y > x = play1 (xs, ys ++ [y, x])

-- True = Player 1 wins, False == Player 2 wins
subGame :: ([Int], [Int]) -> Bool
subGame x = snd $ play2 (S.empty, S.empty) x


play2 :: (S.Set [Int], S.Set [Int]) -> ([Int], [Int]) -> ([Int], Bool)
play2 dones (x, [])      = (x, True)
play2 dones ([], y)      = (y, False)
play2 (xd, yd) (x:xs, y:ys)
    | S.member (x:xs) xd || S.member (y:ys) yd = play2 (S.insert nxs xd, yd) (nxs, ys)
    | n                              = play2Normal (xd, yd) (x:xs, y:ys)
    | otherwise                      = play2Sub (xd, yd) (x:xs, y:ys)
    where
        n = x > length xs || y > length ys
        nxs = xs ++ [x, y]


play2Normal :: (S.Set [Int], S.Set [Int]) -> ([Int], [Int]) -> ([Int], Bool)
play2Normal (xdone, ydone) (x:xs, y:ys)
    | x > y     = play2 (S.insert (x:xs) xdone, ydone) (nxs, ys)
    | otherwise = play2 (xdone, S.insert (y:ys) ydone) (xs , nys)
    where
        nxs = xs ++ [x, y]
        nys = ys ++ [y, x]


play2Sub :: (S.Set [Int], S.Set [Int]) -> ([Int], [Int]) -> ([Int], Bool)
play2Sub (xdone, ydone) (x:xs, y:ys)
    | subGame (xs, ys) = play2 (S.insert (x:xs) xdone, ydone) (nxs, ys)
    | otherwise        = play2 (xdone, S.insert (y:ys) ydone) (xs , nys)
    where
        nxs = xs ++ [x, y]
        nys = ys ++ [y, x]


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (finish played)
    where played = play1 x


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print (finish $ fst played)
    where played = play2 (S.empty, S.empty) x


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
