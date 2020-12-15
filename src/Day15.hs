module Day15
    (solve
    ) where

import Lib ( split, if' )
import qualified Data.Map.Strict as M


-- ((last spoken, spoken time map), length)
type Prep = ((Int, M.Map Int Int), Int)
prepare :: String -> Prep
prepare x = ((head ns, M.fromList (zip (reverse $ tail ns) [1..])), length ns + 1)
    where ns = reverse $ map read $ split (==',') x


step :: (Int, M.Map Int Int) -> Int -> (Int, M.Map Int Int)
step (n, m) t = o
    where
        o = (number, newmap)
        number = if' (n `M.member` m) (t - 1 - (m M.! n)) 0
        newmap = M.insert n (t-1) m


part1 :: Prep -> IO ()
part1 (b, l) = putStr "Part 1: " >> print (fst (foldl step b [l..2020]))


part2 :: Prep -> IO ()
part2 (b, l) = putStr "Part 2: " >> print (fst (foldl step b [l..30000000]))


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
