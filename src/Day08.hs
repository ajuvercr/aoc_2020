module Day08
    (solve
    ) where

import Lib
import NanoParsec
import Control.Monad (liftM2)
import Data.Functor
import Data.Set (Set, insert, empty)

data Instruction = Nop Int
                 | Acc Int
                 | Jmp Int
                 deriving (Show)


parseInstruction :: Parser Instruction
parseInstruction = liftM2 little (token str) (token number)
    where
        little "nop" = Nop
        little "acc" = Acc
        little "jmp" = Jmp


filterInstr :: Instruction -> Bool
filterInstr (Nop x) = x > 0
filterInstr (Jmp x) = x < 0
filterInstr _       = False


changeInstr :: Instruction -> Instruction
changeInstr (Nop x) = Jmp x
changeInstr (Jmp x) = Nop x
changeInstr x       = x


step :: Instruction -> (Int, Int) -> (Int, Int)
step (Acc x) (i, acc) = (i+1, acc+x)
step (Nop _) (i, acc) = (i+1, acc)
step (Jmp x) (i, acc) = (i+x, acc)


data Program = Program { insts :: Prep
                       , done  :: Set Int
                       , state :: (Int, Int)
                       }


currentInstr :: Program -> Instruction
currentInstr p = let (i, _) = state p in
                 insts p !! i


run :: Program -> Either Int Int
run Program { insts=insts, done=done, state=(i, acc)}
    | i `elem` done     = Right acc
    | i >= length insts = Left acc
    | otherwise         = run $ step' (insts !! i) (Program insts done (i, acc))


step' :: Instruction -> Program -> Program
step' ins Program { insts=insts, done=done, state=(i, acc)} = Program insts newdone newstate
    where
        newdone = i `insert` done
        newstate = step ins (i, acc)


bind' :: Program -> Either Int Program
bind' prog
    | filterInstr instr = run (step' instr' prog) $> newprog
    | otherwise         = return newprog
    where
        instr   = currentInstr prog
        instr'  = changeInstr instr
        newprog = step' instr prog


type Prep = [Instruction]
prepare :: String -> Prep
prepare = runParser $ star parseInstruction


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: " >> print (fromRight $ run (Program x empty (0,0)))


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: " >> (print . fromLeft . p2) (Program x empty (0,0))
    where p2 x = bind' x >>= p2


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
