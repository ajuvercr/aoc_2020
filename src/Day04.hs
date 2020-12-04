module Day04
    (solve
    ) where

import NanoParsec
import Data.Char
import Data.Maybe
import Control.Applicative


data Entry = Birth String
    | Issue String
    | Expr String
    | Hgt String
    | Hair String
    | Eye String
    | Pid String
    | Cid String  deriving (Show)

toEntry :: String -> String -> Entry
toEntry "byr" x = Birth x
toEntry "iyr" x = Issue x
toEntry "eyr" x = Expr x
toEntry "hgt" x = Hgt x
toEntry "hcl" x = Hair x
toEntry "ecl" x = Eye x
toEntry "pid" x = Pid x
toEntry "cid" x = Cid x


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


parseEntry :: Parser Entry
parseEntry = do
    key <- str
    char ':'
    toEntry key <$> token strAll


parseEntries :: [String] -> [[Entry]]
parseEntries = map $ mapMaybe (runParserMaybe parseEntry) . split (`elem` " \n")


notcid :: Entry -> Bool
notcid (Cid _) = False
notcid _ = True


isvalid :: [Entry] -> Bool
isvalid xs = 7 == length (filter notcid xs)


validnumber :: (Int -> Bool) -> String -> Bool
validnumber f s
    | all isDigit s = f $ read s
    | otherwise     = False


between :: Int -> Int -> (Int -> Bool)
between min max x = min <= x && x <= max


if' :: Bool -> a -> a -> a
if' True a _ = a
if' False _ b = b


parseHeight :: Parser (Either Int Int)
parseHeight = do
    v <- number
    t <- str
    return $ if' (t == "in") (Left v) (Right v)


validHeight :: Maybe (Either Int Int) -> Bool
validHeight Nothing = False
validHeight (Just (Left x)) = between 59 76 x
validHeight (Just (Right x)) = between 150 193 x


parseTimes :: Int -> Parser a -> Parser ()
parseTimes 0 _ = return ()
parseTimes t p = p >> parseTimes (t-1) p


validHair :: Parser ()
validHair = do
    char '#'
    parseTimes 6 $ digit <|> oneOf "abcdef"


validEye :: String -> Bool
validEye x = x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]


validPid :: Parser ()
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
isvalid2 xs = 7 == length (filter isvalidentry xs)


solve :: String -> IO ()
solve x = do
    let entries = parseEntries $ splitemptyline x
    putStr "Part 1: "
    print $ length $ filter isvalid entries
    putStr "Part 2: "
    print $ length $ filter isvalid2 entries
