module Day18
    (solve
    ) where

import NanoParsec
import Lib
import Control.Applicative
import Debug.Trace

parseMeat :: Parser Int
parseMeat = this <|> parseLit parseMeat -- fallthrough to lit
    where
        this = do
            a <- s2 $ parseLit parseMeat
            f <- ((*) <$ char '*') <|> ((+) <$ char '+')
            b <- s2 parseMeat
            return $ f a b


parseTerm :: Parser Int
parseTerm = this <|> parseTerm' -- fallthrough to term'
    where
        this = do
            a <- s2 parseTerm'
            char '*'
            b <- s2 parseTerm
            return (a * b)


parseTerm' :: Parser Int
parseTerm' = this <|> parseLit parseTerm -- fallthrough to lit
    where
        this = do
            a <- s2 $ parseLit parseTerm
            char '+'
            b <- s2 parseTerm'
            return (a + b)


parseParen :: Parser Int -> Parser Int
parseParen meat = (char ')' *> s2 meat <* char '(') <|> parens meat -- Lol, just reverse string expression :D


parseLit :: Parser Int -> Parser Int
parseLit meat = number <|> parseParen meat


type Prep = String
prepare :: String -> Prep
prepare = id


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (sum . map (runParser parseMeat . reverse) $ lines x)


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print (sum . map (runParser parseTerm) $ lines x)


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
