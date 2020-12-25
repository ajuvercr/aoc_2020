module Day25
    (solve
    ) where

import Lib

transform :: Int -> Int -> Int
transform sub self = self * sub `mod` 20201227


subj :: Int
subj = 7


-- | calculates the loopsize for some public key, subject number and itself
loopSize :: Int -> Int -> Int
loopSize key self = if' (val == key) 1 (1 + loopSize key val)
    where val = transform subj self


type Prep = (Int, Int)
prepare :: String -> Prep
prepare inp = (read x, read y)
    where (x:y:_) = lines inp


part1 :: Prep -> IO ()
part1 (p1, p2) = putStr "Part 1: " >> print (iterate (transform p2) 1 !! loops)
    where loops = loopSize p1 1


part2 :: Prep -> IO ()
part2 x = print "Part 2: "


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
