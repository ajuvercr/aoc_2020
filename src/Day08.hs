module Day08
    (solve
    ) where

import NanoParsec
import Control.Monad (liftM2)

import Data.Set (Set, insert, empty)

data Instruction = Nop Int
                 | Acc Int
                 | Jmp Int
                 deriving (Show)


parseInstruction :: Parser Instruction
parseInstruction = liftM2 little (token str) number
    where
        little "nop" = Nop
        little "acc" = Acc
        little "jmp" = Jmp


exec :: Prep -> Int -> Int -> (Int, Int)
exec is i = step (is !! i) i
    where
        step (Acc x) i acc = (i+1, acc+x)
        step (Nop _) i acc = (i+1, acc)
        step (Jmp x) i acc = (i+x, acc)


fromLeft :: Either a b -> a
fromLeft (Left x) = x


run :: Prep -> Either Int Int
run x = doPart1 x empty 0 0


doPart1 :: Prep -> Set Int -> Int -> Int -> Either Int Int
doPart1 is done i acc
    | i >= length is = Right acc
    | i `elem` done  = Left acc
    | otherwise      = uncurry (doPart1 is newdone) state
    where
        newdone = i `insert` done
        state   = exec is i acc


filterInstr :: Instruction -> Bool
filterInstr (Nop x) = x > 0
filterInstr (Jmp x) = x < 0
filterInstr _       = False


changeInstr :: Instruction -> Instruction
changeInstr (Nop x) = Jmp x
changeInstr (Jmp x) = Nop x
changeInstr x       = x


doPart2 :: Prep -> Prep -> Int
doPart2 b (i:is)
    | filterInstr i = case run (b ++ changeInstr i:is) of
        Right x -> x
        Left  _ -> doPart2 (b ++ [i]) is
    | otherwise     = doPart2 (b ++ [i]) is


type Prep = [Instruction]
prepare :: String -> Prep
prepare = map (runParser parseInstruction) . lines


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (fromLeft $ run x)


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> print (doPart2 [] x)


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
