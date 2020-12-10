module Day10
    (solve
    ) where

import NanoParsec
import Data.List

data State = State { ones :: Int
                   , tres :: Int
                   , llst :: Int
                   } deriving Show


insertJolt :: State -> Int -> State
insertJolt s@(State o r l) n = update (n - l) $ s {llst=n}
    where
        update 1 s = s{ones=o + 1}
        update 2 s = s
        update 3 s = s{tres=r + 1}


doPart2 :: (Int, Int, Int) -> Bool -> (Int, Int, Int)
doPart2 (_, j, k) False = (j, k, 0)
doPart2 (i, j, k) True = (j, k, i + j + k)


prepPart2 :: [Int] -> [Bool]
prepPart2 = sub 1
    where
        sub _ []        = []
        sub i (x:xs)
            | i == x    = True  : sub (i+1) xs
            | otherwise = False : sub (i+1) (x:xs)


parsePrep :: Parser [Int]
parsePrep = star $ token number


type Prep = [Int]
prepare :: String -> Prep
prepare = sort . runParser parsePrep


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (extract $ foldl insertJolt (State 0 0 0) x)
    where extract (State o t _)= o * (t+1)


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print l
    where (_, _, l) = foldl doPart2 (0, 0, 1) $ prepPart2 x


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
