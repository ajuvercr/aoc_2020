module Day06
    (solve
    ) where

import Lib
import Data.Set (Set, fromList, size, union, empty, intersection)


parseEntries :: String -> [Set Char]
parseEntries = map fromList . lines


doTheThing :: (Set Char -> Set Char -> Set Char) -> Set Char -> Prep -> Int
doTheThing f init = sum . map (size . foldl f init)


alphaSet :: Set Char
alphaSet = fromList "abcdefghijklmnopqrstuvwxyz"


type Prep = [[Set Char]]
prepare :: String -> Prep
prepare = map parseEntries . splitemptyline


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> (print . doTheThing union empty) x


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> (print . doTheThing intersection alphaSet) x


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
