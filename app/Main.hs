module Main where

import Control.Monad
import Control.Applicative
import System.IO
import System.Environment

import Data.Time.Clock
import Data.Time.Calendar

import qualified Day01

thdOf3 :: (a, b, c) -> c
thdOf3 (_, _, x) = x


date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = toGregorian . utctDay <$> getCurrentTime

dayofmonth :: IO Int
dayofmonth = thdOf3 <$> date


parse :: [String] -> IO (Int, String)
parse (location:xs) = liftM2 (,) (parseDay xs) (readfile location)
parse [] = liftM2 (,) dayofmonth getContents


parseDay :: [String] -> IO Int
parseDay [x] = return $ read x
parseDay _ = dayofmonth


readfile :: String -> IO String
readfile location = openFile location ReadMode >>= hGetContents


main :: IO ()
main = getArgs >>=  parse >>= uncurry solve


solve :: Int -> String -> IO ()
solve 1 = Day01.solve
