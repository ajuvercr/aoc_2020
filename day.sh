#!/usr/bin/env bash

day=$(date '+%d' | sed 's/^0//')
daylong=$(date '+%d')
importline=$(($day+8))
lastline=$(wc -l < app/Main.hs)
lastline=$(($lastline + 1))

curl --cookie "session=$AOCTOKEN" https://adventofcode.com/2020/day/$day/input > res/$daylong.txt
sed -i "${importline}iimport qualified Day$daylong" app/Main.hs
echo "solve $day = Day$daylong.solve" >> app/Main.hs

cat <<EOF > src/Day$daylong.hs
module Day$daylong
    (solve
    ) where

    solve :: String -> IO ()
    solve x = return ()
EOF
