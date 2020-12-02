module Day02
    (solve
    ) where

import Data.List
import Control.Applicative

parseinput :: String -> Maybe [(Char, Int, Int, [Char])]
parseinput x = parselines $ lines x

parselines :: [String] -> Maybe [(Char, Int, Int, [Char])]
parselines [] = return []
parselines (x:xs) = do
    line <- parseline x
    xs   <- parselines xs
    return $ line : xs

parseline :: String -> Maybe (Char, Int, Int, [Char])
parseline s = do
    let (min, s1) = split '-' s
    let (max, s2) = split ' ' s1
    let (le, s3) = split ':' s2
    return (head le, read min, read max, tail s3)

split :: Char -> String -> (String, String)
split c s = (firstWord, rest)
    where
        firstWord = takeWhile (/=c) s
        rest = drop (length firstWord + 1) s

count :: Char -> String -> Int
count c s = length $ filter (==c) s

iscorrect :: (Char, Int, Int, [Char]) -> Bool
iscorrect (t, min, max, s) = min <= c && c <= max
    where c = count t s

countcorrect :: [(Char, Int, Int, [Char])] -> Int
countcorrect [] = 0
countcorrect (e:xs)
    | iscorrect e = 1 + countcorrect xs
    | otherwise   = countcorrect xs


xor :: Bool -> Bool -> Bool
xor True True = False
xor True _    = True
xor _    True = True
xor _    _    = False


iscorrect2 :: (Char, Int, Int, [Char]) -> Bool
iscorrect2 (t, min, max, s) = xor (t == s !! (min - 1)) (t == at s !! (max - 1))


countcorrect2 :: [(Char, Int, Int, [Char])] -> Int
countcorrect2 [] = 0
countcorrect2 (e:xs)
    | iscorrect2 e = 1 + countcorrect2 xs
    | otherwise   = countcorrect2 xs


solve :: String -> IO ()
solve x = do
    print $ countcorrect <$> parseinput x
    print $ countcorrect2 <$> parseinput x
