module Main where

import System.IO
import System.Environment

import Data.Time.Clock
import Data.Time.Calendar

import qualified Day01
import qualified Day02

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


parse :: [String] -> IO (Int, String)
parse args = do
    day <- parseDay $ safeHead args
    c <- readfile day $ safeHead . safeTail $ args
    return (day, c)


parseDay :: Maybe String -> IO Int
parseDay (Just x) = return $ read x
parseDay Nothing = dayofmonth


dayFileLocation :: Int -> String
dayFileLocation day
    | day < 10 = "res/0" ++ show day ++ ".txt"
    | otherwise = "res/" ++ show day ++ ".txt"


readfile :: Int -> Maybe String -> IO String
readfile _ (Just "--") = getContents
readfile _ (Just x) = openFile x ReadMode >>= hGetContents
readfile day Nothing = openFile (dayFileLocation day) ReadMode >>= hGetContents


main :: IO ()
main = getArgs >>= parse >>= uncurry solve

solve :: Int -> String -> IO ()
solve 1 = Day01.solve
solve 2 = Day02.solve
