{-# LANGUAGE TupleSections #-}
module Main where

import System.IO
import System.Environment

import Data.Time.Clock
import Data.Time.Calendar

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

thdOf3 :: (a, b, c) -> c
thdOf3 (_, _, x) = x


date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = toGregorian . utctDay <$> getCurrentTime


dayofmonth :: IO Int
dayofmonth = thdOf3 <$> date


safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing


safeTail :: [a] -> [a]
safeTail (_:xs) = xs
safeTail []     = []


parse :: [String] -> IO (Int, Maybe Int, String)
parse [file, day, part] = (read day, Just $ read part, ) <$> readfile (read day) (Just file)
parse [file, day] = (read day, Nothing, ) <$> readfile (read day) (Just file)
parse [file] = dayofmonth >>= \day -> (day, Nothing, ) <$> readfile day (Just file)
parse [] = dayofmonth >>= \day -> (day, Nothing, ) <$> readfile day Nothing


dayFileLocation :: Int -> String
dayFileLocation day
    | day < 10 = "res/0" ++ show day ++ ".txt"
    | otherwise = "res/" ++ show day ++ ".txt"


readfile :: Int -> Maybe String -> IO String
readfile _   (Just "--") = getContents
readfile day (Just "!")  = openFile (dayFileLocation day) ReadMode >>= hGetContents
readfile _   (Just x)    = openFile x ReadMode >>= hGetContents
readfile day Nothing     = openFile (dayFileLocation day) ReadMode >>= hGetContents


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a,b,c) = f a b c


main :: IO ()
main = getArgs >>= parse >>= uncurry3 solve


solve :: Int -> Maybe Int -> String -> IO ()
solve 1 = Day01.solve
solve 2 = Day02.solve
solve 3 = Day03.solve
solve 4 = Day04.solve
solve 5 = Day05.solve
solve 6 = Day06.solve
solve 7 = Day07.solve
solve 8 = Day08.solve
solve 9 = Day09.solve
solve 10 = Day10.solve
solve 11 = Day11.solve
solve 12 = Day12.solve
solve 13 = Day13.solve
solve 14 = Day14.solve
solve 15 = Day15.solve
solve 16 = Day16.solve
solve 17 = Day17.solve
solve 18 = Day18.solve
solve 19 = Day19.solve
solve 20 = Day20.solve
solve 21 = Day21.solve
solve 22 = Day22.solve
solve 23 = Day23.solve
solve 24 = Day24.solve
solve 25 = Day25.solve
