module Day16
    (solve
    ) where

import Lib
import NanoParsec
import Control.Applicative
import Data.List
import Data.Set(Set)
import qualified Data.Set as S


type Range = (Int, Int)
parseRange :: Parser Range
parseRange = (,) <$> number <*> (char '-' >> number)

parseRanges :: Parser [Range]
parseRanges = delimitered " or " parseRange

inRange :: Range -> Int -> Bool
inRange (l, r) x = x >= l && x <= r

inRanges :: [Range] -> Int -> Bool
inRanges rules x = any (`inRange` x) rules


type Rule = (String, [Range])
parseRule :: Parser Rule
parseRule = do
    name <- plus (satisfy (/=':'))
    reserved  ":"
    ranges <- parseRanges
    return (name, ranges)

inRule :: Rule -> Int -> Bool
inRule (_, ranges) = inRanges ranges

inRules :: [Rule] -> Int -> Bool
inRules rs x = any (`inRule` x) rs


type Ticket = [Int]
parseTicket :: Parser Ticket
parseTicket = delimitered  "," number


invalid :: [Rule] -> Ticket -> [Int]
invalid rs = filter (not . inRules rs)

isinvalid :: [Rule] -> Ticket -> Bool
isinvalid rs = not . all (inRules rs)


isvalid :: [Rule] -> Ticket -> Bool
isvalid rs = all (inRules rs)


validFor :: Rule -> [Int] -> Bool
validFor r = all (inRule r)


validAts :: [Rule] -> [Ticket] -> [[Rule]]
validAts rs ts = map (\col -> filter (`validFor` col) rs) (transpose  ts)


nameValidAts :: [Rule] -> [Ticket] -> [[String]]
nameValidAts rs ts = map (map fst) (validAts rs ts)


correctOrder :: Int -> Set String -> [[String]] -> Maybe [String]
correctOrder _ _ []      = Just []    -- base case
correctOrder _ _ ([]:ls) = Nothing    -- no more options for the current slice
correctOrder l done ((x:xs):ls)
    | x `S.member` done = cont      -- This option is already looked at, so yeah
    | otherwise         = try <|> cont
    where
        try = if' (length done + length ls > l) Nothing ((x:) <$> correctOrder l (x `S.insert` done) ls)
        cont = correctOrder l done (xs:ls)


type Prep = ([Rule], Ticket, [Ticket])
parsePrep :: Parser Prep
parsePrep = do
    rules <- plus (parseRule <* spaces)
    string "your ticket:" >> spaces
    t <- parseTicket <* spaces

    string "nearby tickets:" >> spaces
    ts <- plus (parseTicket <* spaces)

    return (rules, t, ts)


startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys) = (x == y) && startsWith xs ys


part2solend :: Maybe [String] -> Ticket -> Int
part2solend (Just rs) ticket = product $ zipWith dothing rs ticket
    where
        dothing r i
            | startsWith "departure" r = i
            | otherwise                = 1

prepare :: String -> Prep
prepare = runParser parsePrep


part1 :: Prep -> IO ()
part1 (rules, _, tickets) = putStr "Part 1: " >> print sol
    where sol = sum (concatMap (invalid rules) tickets)


part2 :: Prep -> IO ()
part2 (rules, my_ticket, tickets) = putStr "Part 2: " >> print (part2solend correct your_ticket)
    where
        valids = filter (isvalid rules) tickets
        vs = nameValidAts rules valids
        sorter (a, _) (b, _) = compare (length a) (length b)
        (vs', your_ticket) = (unzip . sortBy sorter) (zip vs my_ticket)
        correct = correctOrder (length rules) S.empty vs'


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
