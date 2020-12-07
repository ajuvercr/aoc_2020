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
parseReqs = multiple parseReq


parseBag :: Parser Bag
parseBag = do
    name <- parseBagName
    reserved "contain"
    reqs <- parseReqs <|> (reserved "no other bags." >> return [])
    return $ Bag name reqs


validBag :: Bag -> Set String -> Bool
validBag Bag {color=_, req=req} vs = all invs req
    where invs (_, n) = Set.member n vs


insertAll :: Ord a => Set a -> [a] -> Set a
insertAll = foldl (flip Set.insert)


partitionReqs :: [Bag] -> Set String -> ([Bag], [Bag])
partitionReqs (b:bs) valid
    | validBag b valid = (bad, b:good)
    | otherwise        = (b:bad, good)
    where (bad, good) = partitionReqs bs valid
partitionReqs [] _ = ([], [])


sortReqs :: [Bag] -> Set String -> [Bag]
sortReqs [] _ = []
sortReqs bs valid = let (bad, good) = partitionReqs bs valid
                    in good ++ sortReqs bad (insertAll valid (map color good))


containsShiny :: Bag -> Set String -> Bool
containsShiny b valid = any invs (req b)
    where invs (_, n) = n `elem` valid


shinies :: [Bag] -> Set String -> Set String
shinies (b:bs) valid
    | containsShiny b valid = shinies bs (Set.insert (color b) valid)
    | otherwise             = shinies bs valid
shinies [] valid = valid


countBags :: Map String Int -> Bag -> Map String Int
countBags m Bag {color=name, req=req} = Map.insert name c m
    where
        c = 1 + sum (map (\(t, n) -> t * m Map.! n) req)


type Prep = [Bag]
prepare :: String -> Prep
prepare x = b
    where
        bags = map (runParser parseBag) $ lines x
        b = sortReqs bags Set.empty


part1 :: Prep -> IO ()
part1 x = do
    putStr "Part 1: "
    print $ length (shinies x $ Set.fromList ["shiny gold"]) - 1    -- Initial shiny gold bag


part2 :: Prep -> IO ()
part2 x = do
    putStr "Part 2: "
    let folded = foldl countBags Map.empty x :: Map String Int
    print $ folded Map.! "shiny gold" -1


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
