module Day14
    (solve
    ) where

import Lib

import Data.Bits
import NanoParsec
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import Control.Applicative

import Data.Char (digitToInt)
import Data.List (foldl')

type Mask = ((Int, Int, Mask2), [(Int, Int)])


data Action = Store Int Int
            | Mask Mask


type Mask2 = String

newtype Masker = Masker { valid :: Mask2 -> (Int, Mask2) }

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0


orElse :: Maybe a -> a -> a
orElse Nothing  x = x
orElse (Just x) _ = x

orElse' :: Maybe a -> a -> Maybe a
orElse' Nothing x = Just x
orElse' x _ = x

-- REVERSED!
tobitstring :: Int -> String
tobitstring 0 = ""
tobitstring x = show d ++ tobitstring rest
    where (rest, d) = x `divMod` 2


tomask :: Int -> Mask2
tomask x = r ++ replicate (36 - l) '0'
    where
        r = tobitstring x
        l = length r


withMask :: Mask2 -> Mask2 -> Mask2
withMask [] [] = []
withMask (x:xs) (y:ys)
    | y == 'X' = 'X' : withMask xs ys
    | y == '1' = '1' : withMask xs ys
    | otherwise = x  : withMask xs ys


other :: Char -> Char
other '0' = '1'
other '1' = '0'


applyMask :: Mask2 -> Mask2 -> Maybe Mask2
applyMask [] [] = Nothing
applyMask (x:xs) (m:ms)
    | m == 'X'  = (x:) <$> applyMask xs ms
    | x == 'X'  = (('X':) <$> applyMask xs ms) `orElse'` (other m:xs)
    | x == m    = (x:) <$> applyMask xs ms
    | otherwise = Just (x:xs)


applyMasks :: [Mask2] -> Mask2 -> Maybe Mask2
applyMasks [] x = Just x
applyMasks (m:ms) x = applyMask x m >>= applyMasks ms


dopart2' :: Mask2 -> (Int, Int) -> (Int, [Mask2]) -> (Int, [Mask2])
dopart2' m (k, v) (s, ms) = newMask `orElse` (s, ms)
    where
        mask = withMask (tomask k) m
        domask m = (s + (2 ^ count (=='X') m) * v, m:ms)
        newMask = domask <$> applyMasks (reverse ms) mask


domasks :: Mask2 -> [Mask2]
domasks [] = [""]
domasks (m:ms)
    | m == 'X' = map ('0':) rest ++ map ('1':) rest
    | otherwise = map (m:) rest
    where rest = domasks ms


dopart2'' :: Mask2 -> M.Map Int Int -> (Int, Int) -> M.Map Int Int
dopart2'' mask m (k, v) = install m (domasks k'')
    where
        k'' = withMask (tomask k) mask
        install s [] = s
        install s (x:xs) = M.insert (toDec x) v (install s xs)


dopart2 :: [Mask] -> (Int, [Mask2])
dopart2 [] = (0, [])
dopart2 (((_, _, mask), mses):ms) = foldr (dopart2' mask) (dopart2 ms) (reverse mses)


dopart22 :: M.Map Int Int -> Mask -> M.Map Int Int
dopart22 s ((_, _, mask), mses) = foldl (dopart2'' mask) s mses


toint :: (a -> Bool) -> [a] -> Int
toint _ []     = 0
toint p (x:xs)
    | p x       = 1 + 2 * toint p xs
    | otherwise = 2 * toint p xs


parseStore :: Parser (Int, Int)
parseStore = do
    string "mem["
    loc <- number
    string "] = "
    (,) loc <$> number <* spaces


parseMask :: Parser Mask
parseMask = do
    string "mask = "
    v <- reverse <$> strAll
    spaces
    xs <- plus parseStore
    return ((toint (=='1') v, complement (toint (=='0') v), v), xs)


doStep :: M.Map Int Int -> Mask -> M.Map Int Int
doStep s ((ls, os, _), xs) = foldl ins s xs
    where
        ins s (at, v) = M.insert at (mask v) s
        mask v = os .&. v .|. ls


type Prep = [Mask]
prepare :: String -> Prep
prepare = runParser (star parseMask)


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (sum (M.elems sol))
    where sol = Prelude.foldl doStep M.empty x


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print (sum (M.elems sol))
    where sol = foldl dopart22 M.empty x



solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
