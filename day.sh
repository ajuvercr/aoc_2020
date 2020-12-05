#!/usr/bin/env bash

day=$(date '+%d' | sed 's/^0//')
daylong=$(date '+%d')
importline=$(($day+9))
lastline=$(wc -l < app/Main.hs)
lastline=$(($lastline + 1))

curl --cookie "session=$AOCTOKEN" https://adventofcode.com/2020/day/$day/input > res/$daylong.txt
sed -i "${importline}iimport qualified Day$daylong" app/Main.hs
echo "solve $day = Day$daylong.solve" >> app/Main.hs

cat <<EOF > src/Day$daylong.hs
module Day$daylong
    (solve
    ) where

import Lib


type Prep = String
prepare :: String -> Prep
prepare = id


part1 :: Prep -> IO ()
part1 x = putStr "Part 1: "


part2 :: Prep -> IO ()
part2 x = putStr "Part 2: "


solve :: Maybe Int -> String -> IO ()
solve (Just 1) x = part1 $ prepare x
solve (Just 2) x = part2 $ prepare x
solve _        x = part1 prep >> part2 prep
    where prep = prepare x
EOF
