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
