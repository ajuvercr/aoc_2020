module Lib where


printArray :: Show a => [a] -> IO ()
printArray = foldMap print


splitemptyline :: String -> [String]
splitemptyline "" = [""]
splitemptyline ('\n':'\n':xs) = "" : splitemptyline xs
splitemptyline (x:xs) = (x:current):rest
    where (current:rest) = splitemptyline xs


split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = [[]]
split f (x:xs)
    | f x       = [] : split f xs
    | otherwise = (x : h) : rest
    where (h:rest) = split f xs



if' :: Bool -> a -> a -> a
if' True a _ = a
if' False _ b = b


between :: Int -> Int -> (Int -> Bool)
between min max x = min <= x && x <= max


count :: (a -> Bool) -> [a] -> Int
count f = length . filter f


fromLeft :: Either a b -> a
fromLeft (Left x) = x

fromRight :: Either a b -> b
fromRight (Right x) = x
