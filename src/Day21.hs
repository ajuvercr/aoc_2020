module Day21
    (solve
    ) where

import Lib
import NanoParsec
import qualified Data.Set as S
import Control.Applicative
import Data.Bifunctor
import Data.List

type Food = (S.Set String, S.Set String)

parseFood :: Parser Food
parseFood = do
    ing <- star (str <* spaces)
    con <- parens (reserved "contains" *> delimitered ", " str)
    return (S.fromList ing, S.fromList con)


correctOrder :: S.Set String -> [(String, [String])] -> Maybe [(String, String)]
correctOrder _ [] = Just []
correctOrder _ ((_, []):xs) = Nothing
correctOrder used ((aleg, i:is):xs)
    | i `S.member` used = correctOrder used ((aleg, is):xs)
    | otherwise         = (((aleg, i):) <$> correctOrder (S.insert i used) xs) <|> correctOrder used ((aleg, is):xs)



unsplit :: String -> [String] -> String
unsplit _ [x] = x
unsplit s (x:xs) = x ++ s ++ unsplit s xs


type Prep = ([String], [(String, S.Set String)])
prepare :: String -> Prep
prepare s = (ing, map getFood' (S.toList total))
    where
        x = runParser (star $ parseFood <* spaces) s
        ing   = foldl1 (++) (map (S.toList .fst) x)
        total = foldl1 S.union (map snd x)
        getFood food = foldl1 S.intersection (map fst $ filter ((food `S.member`) . snd) x)
        getFood' x = (x, getFood x)
        alegens = foldl1 S.union (map getFood (S.toList total))


part1 :: Prep -> IO ()
part1 (ing, prod) = putStr "Part 1: " >> print (count (`S.notMember` alegens) ing)
    where
        alegens = foldl1 S.union (map snd prod)


part2 :: Prep -> IO ()
part2 (ing, prod) = putStr "Part 2: " >> print (unsplit "," canonical)
    where
        dangerous = unwrap $ correctOrder S.empty (map (Data.Bifunctor.second S.toList) prod)
        canonical = map snd $ sort dangerous

solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
