{-# LANGUAGE TupleSections #-}

module Day19
    (solve
    ) where

import Lib
import NanoParsec
import Control.Applicative
import Data.Char
import qualified Data.Map.Strict as M


data Rule = Lit Char
          | Comb [[Int]]
          deriving (Show)


parseLit :: Parser Rule
parseLit = Lit <$> (char '"' *> satisfy isAlpha <* char '"')


parseComb :: Parser Rule
parseComb = Comb <$> delimitered "|" (plus $ number <* spaces')


parseRule :: Parser (Int, Rule)
parseRule = do
    at <- number
    reserved ":"
    (at,) <$> (parseLit <|> parseComb)


parseInput :: Parser Prep
parseInput = do
    rules <- plus (parseRule <* spaces)
    strs  <- star (str <* spaces)
    return (M.fromList rules, strs)


step :: Rules -> Int -> String -> [String]
step rs i s
    | (Comb paths) <- rule = concatMap (doPath rs s) paths
    | (Lit x)      <- rule = if' (isEmpty s || x /= head s) [] [tail s]
    where rule = rs M.! i


doPath :: Rules -> String -> [Int] -> [String]
doPath rs str = foldl ss [str]
    where
        ss strs x = concatMap (step rs x) strs


isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False


type Rules = M.Map Int Rule
type Prep = (Rules, [String])
prepare :: String -> Prep
prepare = runParser parseInput


part1 :: Prep -> IO ()
part1 (rules, tests) = putStr "Part 1: " >> print (count id (map doString tests))
    where
        f = step rules 0
        doString s = count isEmpty (f s) == 1

-- 8: 42 | 42 8
-- 11: 42 31 | 42 11 31
part2 :: Prep -> IO ()
part2 (rules, tests) = putStr "Part 2: " >> print (count id (map doString tests))
    where
        newrs = M.insert 8 (Comb [[42], [42, 8]]) (M.insert 11 (Comb [[42, 31], [42, 11, 31]]) rules)
        f = step newrs 0
        doString s = count isEmpty (f s) == 1


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
