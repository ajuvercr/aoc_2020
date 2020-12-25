{-# LANGUAGE TupleSections #-}

module Day25
    (solve
    ) where

import Lib

transform :: Int -> Int -> Int
transform = curry ((`mod` 20201227) . uncurry (*))

loops :: Int -> (Int, Int)
loops key = fromLeft $ loops' 1 (map (1,) [1..])
    where
        loops' count state = tloop count state >>= loops' (count+1)
        tloop 0 xs = Right xs
        tloop c (x:xs) = (:) <$> goodOrTransform c x <*> tloop (c-1) xs
        goodOrTransform c (self, sub) = let self' = transform sub self in if' (self' == key) (Left (sub, c)) (Right (self', sub))


type Prep = (Int, Int)
prepare :: String -> Prep
prepare inp = (read x, read y)
    where (x:y:_) = lines inp


part1 :: Prep -> IO ()
part1 (p1, p2) = putStr "Part 1: " >> print (iterate (transform p2) 1 !! l1)
    where
        (s1, l1) = loops p1
        (s2, l2) = loops p2
    -- where loopSize =


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print secret -- , secret == 14897079)
    where
        (s1, l1) = loops 5764801
        (s2, l2) = loops 17807724
        secret = take (l1+1) $ iterate (transform 17807724) 1


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
