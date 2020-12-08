module Day07
    (solve
    ) where

import NanoParsec
import Control.Applicative
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)


data Bag = Bag { color :: String
               , req   :: [(Int, String)] } deriving (Show)


parseBagName :: Parser String
parseBagName = do
    adj <- token str
    color <- token str
    reserved "bags" <|> reserved "bag"
    return $ adj ++ " " ++ color


parseReq :: Parser (Int, String)
parseReq = do
    c <- token number
    name <- parseBagName
    token $ oneOf ".,"
    return (c, name)


parseReqs :: Parser [(Int, String)]
parseReqs = plus parseReq


parseBag :: Parser Bag
parseBag = do
    name <- parseBagName
    reserved "contain"
    reqs <- parseReqs <|> (reserved "no other bags." >> return [])
    spaces
    return $ Bag name reqs


allInValid :: Bag -> Set String -> Bool
allInValid b valid = all invs (req b)
    where invs (_, n) = n `elem` valid


someInValid :: Bag -> Set String -> Bool
someInValid b valid = any invs (req b)
    where invs (_, n) = n `elem` valid


insertAll :: Ord a => Set a -> [a] -> Set a
insertAll = foldl (flip Set.insert)


partitionReqs :: [Bag] -> Set String -> ([Bag], [Bag])
partitionReqs (b:bs) valid
    | allInValid b valid = (bad, b:good)
    | otherwise        = (b:bad, good)
    where (bad, good) = partitionReqs bs valid
partitionReqs [] _ = ([], [])


sortReqs :: [Bag] -> Set String -> [Bag]
sortReqs [] _ = []
sortReqs bs valid = let (bad, good) = partitionReqs bs valid
                    in good ++ sortReqs bad (insertAll valid (map color good))


shinies :: [Bag] -> Set String -> Set String
shinies [] valid = valid
shinies (b:bs) valid
    | someInValid b valid = shinies bs (Set.insert (color b) valid)
    | otherwise           = shinies bs valid


countBags :: Map String Int -> Bag -> Map String Int
countBags m Bag {color=name, req=req} = Map.insert name c m
    where c = 1 + sum (map (\(t, n) -> t * m Map.! n) req)


type Prep = [Bag]
prepare :: String -> Prep
prepare = sorted . runParser (star parseBag)
    where
        sorted bags = sortReqs bags Set.empty


part1 :: Prep -> IO ()
part1 x = do
    putStr "Part 1: "
    print $ length (shinies x $ Set.fromList ["shiny gold"]) - 1    -- Initial shiny gold bag


part2 :: Prep -> IO ()
part2 x = do
    putStr "Part 2: "
    let folded = foldl countBags Map.empty x :: Map String Int
    print $ folded Map.! "shiny gold" -1 -- Initial shiny gold bag


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
