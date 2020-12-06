module Day04
    (solve
    ) where

import Lib
import NanoParsec
import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Monad (liftM2)

data Entry = Birth String
    | Issue String
    | Expr String
    | Hgt String
    | Hair String
    | Eye String
    | Pid String
    | Cid String  deriving (Show)


parseEntry :: Parser Entry
parseEntry = byr <|> iyr <|> eyr <|> hgt <|> hcl <|> ecl <|> pid <|> cid
    where
        little x f = string x >> char ':' >> f <$> token strAll
        byr = little "byr" Birth
        iyr = little "iyr" Issue
        eyr = little "eyr" Expr
        hgt = little "hgt" Hgt
        hcl = little "hcl" Hair
        ecl = little "ecl" Eye
        pid = little "pid" Pid
        cid = little "cid" Cid


parseEntries :: [String] -> [[Entry]]
parseEntries = map $ runParser $ multiple parseEntry


notcid :: Entry -> Bool
notcid (Cid _) = False
notcid _ = True


isvalid :: [Entry] -> Bool
isvalid xs = 7 == count notcid xs


validnumber :: (Int -> Bool) -> String -> Bool
validnumber f s
    | all isDigit s = f $ read s
    | otherwise     = False


parseHeight :: Parser (Int, String)
parseHeight = liftM2 (,) number str


validHeight :: Maybe (Int, String) -> Bool
validHeight Nothing = False
validHeight (Just (x, "in")) = between 59 76 x
validHeight (Just (x, "cm")) = between 150 193 x


validHair :: Parser ()
validHair = char '#' >> parseTimes 6 (digit <|> oneOf "abcdef") >> return ()


validEye :: String -> Bool
validEye x = x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]


validPid :: Parser String
validPid = parseTimes 9 digit


isvalidentry :: Entry -> Bool
isvalidentry (Birth s) = validnumber (between 1920 2002) s
isvalidentry (Issue s) = validnumber (between 2010 2020) s
isvalidentry (Expr s)  = validnumber (between 2020 2030) s
isvalidentry (Hgt s)   = validHeight $ runParserMaybe parseHeight s
isvalidentry (Hair s)  = isJust $ runParserMaybe validHair s
isvalidentry (Eye s)   = validEye s
isvalidentry (Pid s)   = isJust $ runParserMaybe validPid s
isvalidentry (Cid _)   = False


isvalid2 :: [Entry] -> Bool
isvalid2 xs = 7 == count isvalidentry xs


type Prep = [[Entry]]
prepare :: String -> Prep
prepare = parseEntries . splitemptyline


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> (print . count isvalid) x


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> (print . count isvalid2) x


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
