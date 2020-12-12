module Day12
    (solve
    ) where

import Control.Monad (liftM2)
import NanoParsec

sin' :: Int -> Int
sin' 0 = 0
sin' 180 = 0
sin' 90 = 1
sin' 270 = -1
sin' x
    | x < 0   = sin' $ x + 360
    | x >= 360 = sin' $ x - 360
    | otherwise = error ("Wtf is this angle " ++ show x)


cos' :: Int -> Int
cos' 0 = 1
cos' 180 = -1
cos' 90 = 0
cos' 270 = 0
cos' x
    | x < 0   = cos' $ x + 360
    | x >= 360 = cos' $ x - 360
    | otherwise = error ("Wtf is this angle " ++ show x)


data Special = Angle Int
             | WayPoint (Int, Int)


type Ship = (Int, Int, Special)


type Move = (Char, Int)


parseMove :: Parser Move
parseMove = liftM2 (,) item number <* spaces


specialMove1 :: (Int, Int, Int) -> Move -> Ship
specialMove1 (x, y, an) ('L', a) = (x, y, Angle $ an + a)
specialMove1 (x, y, an) ('R', a) = (x, y, Angle $ an - a)
specialMove1 (x, y, angle) ('F', a) = (x+c*a, y+s*a, Angle angle)
    where
        c = cos' angle
        s = sin' angle


specialMove2 :: (Int, Int, (Int, Int)) -> Move -> Ship
specialMove2 (x, y, (x', y')) ('F', a) = (x, y, WayPoint (x' + x*a, y' + y*a))
specialMove2 (x, y, ship) (m, a) = (c*x+(-s)*y, s*x+c*y, WayPoint ship)
    where
        s | m == 'L'  = sin' a
          | otherwise = sin' (360 - a)
        c | m == 'L'  = cos' a
          | otherwise = cos' (360 - a)


applyMove :: Ship -> Move -> Ship
applyMove (x, y, s) ('N', a) = (x,   y+a, s)
applyMove (x, y, s) ('S', a) = (x,   y-a, s)
applyMove (x, y, s) ('W', a) = (x-a, y,   s)
applyMove (x, y, s) ('E', a) = (x+a, y,   s)
applyMove (x, y, Angle a) m = specialMove1 (x, y, a) m
applyMove (x, y, WayPoint s) m = specialMove2 (x, y, s) m



manHatten :: Ship -> Int
manHatten (x, y, Angle _)         = abs x + abs y
manHatten (_, _, WayPoint (x, y)) = abs x + abs y


type Prep = [Move]
prepare :: String -> Prep
prepare = runParser (star parseMove)


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (manHatten (foldl applyMove (0, 0, Angle 0) x))


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print (manHatten (foldl applyMove (10, 1, WayPoint (0,0)) x))


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
