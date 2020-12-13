module Day13
    (solve
    ) where

import Data.Maybe ( mapMaybe )
import Util


data Bus = Time Integer
         | X
         deriving (Show)


-- Rosetta code
modInv :: Integer -> Integer -> Integer
modInv a m
  | 1 == g = mkPos i
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x


gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)


step :: (Integer, Integer) -> Bus -> (Integer, Integer)
step x X = step x (Time 1)
step (g, o) (Time x) = (g * x, x * tok g x (o + 1))
    where tok x y o = (o * modInv y x) `mod` x


parseBus :: String -> Bus
parseBus "x" = X
parseBus  x = Time $ read x



filterPart :: Integer -> Bus -> Maybe (Integer, Integer)
filterPart _ X        = Nothing
filterPart x (Time t) = Just (t - x `mod` t, t)


type Prep = (Integer, [Bus])
prepare :: String -> Prep
prepare x = let h:hs:_ = lines x
            in (read h, map parseBus $ split ',' hs)


part1 :: Prep -> IO ()
part1 (target, busses) = putStr "Part 1: " >> print (uncurry (*) (foldl min (target, 0) (mapMaybe (filterPart target) busses)))


part2 :: Prep -> IO ()
part2 (_, busses) = putStr "Part 2: " >> print sol
    where
        l = toInteger $ length busses
        sol = 1 - l + snd (foldl step (1, 0) busses)


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
