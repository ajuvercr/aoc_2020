module Day13
    (solve
    ) where

import Lib
import NanoParsec ( Parser, runParser, char, string, star, number )
import Control.Applicative
import Data.Functor (($>))
import Data.Maybe

data Bus = Time Int
         | X
         deriving (Show)

parseBus :: Parser Bus
parseBus = (time <|> none) <* (char ',' $> () <|> return ())
    where
        time = Time <$> number
        none = X <$ string "x"


filterPart :: Int -> Bus -> Maybe (Int, Int)
filterPart _ X        = Nothing
filterPart x (Time t) = Just (t - x `mod` t, t)


type Prep = (Int, [Bus])
prepare :: String -> Prep
prepare x = let h:hs:_ = lines x
            in (read h, runParser (star parseBus) hs)


part1 :: Prep -> IO ()
part1 (target, busses) = putStr "Part 1: " >> print (uncurry (*) (foldl min (target, 0) (mapMaybe (filterPart target) busses)))


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: "


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
