module Day14
    (solve
    ) where

import Lib

import Data.Bits
import NanoParsec
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace

type Mask = (Int, Int, Mask2)

data Action = Store Int Int
            | Mask Mask


type Mask2 = String
-- data MaskMask =


toint :: (a -> Bool) -> [a] -> Int
toint _ []     = 0
toint p (x:xs)
    | p x       = 1 + 2 * toint p xs
    | otherwise = 2 * toint p xs




parseStore :: Parser Action
parseStore = do
    string "mem["
    loc <- number
    string "] = "
    Store loc <$> number <* spaces


parseMask :: Parser Action
parseMask = do
    string "mask = "
    v <- reverse <$> strAll
    spaces
    return $ Mask (toint (=='1') v, complement (toint (=='0') v), v)


doStep :: (M.Map Int Int, Mask) -> Action -> (M.Map Int Int, Mask)
doStep (s, (ls, os, xs)) (Store at v) = (M.insert at v' s , (ls, os, xs))
    where v' = os .&. v .|. ls
doStep (s, _) (Mask x) = (s, x)


doStep2 :: (M.Map Int Int, Mask) -> Action -> (M.Map Int Int, Mask)
doStep2 (s, _) (Mask x) = (s, x)
-- doStep2 (s, (ls, os, xs)) (Store at v) = (setAll at' v xs s, (ls, os, xs))
--     where at' = os .&. at .|. ls


type Prep = [Action]
prepare :: String -> Prep
prepare = runParser (star (parseStore `option` parseMask))


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (sum (fst sol))
    where sol = Prelude.foldl doStep (M.empty, (0, 0, [])) x


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print (sum (fst sol))
    where sol = Prelude.foldl doStep2 (M.empty, (0, 0, [])) x



solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
