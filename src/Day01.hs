module Day01
    ( solve
    ) where
import Data.List
import Control.Applicative


parseinput :: String -> [Int]
parseinput content = map read $ lines content


solve1 :: Int -> [Int] -> [Int] -> Maybe Int
solve1 target (s:ss) (l:ll)
    | s + l < target = solve1 target ss (l:ll)
    | s + l > target = solve1 target (s:ss) ll
    | otherwise      = return $ s * l
solve1 _ _ _ = Nothing


firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f (x:xs) = f x <|> firstJust f xs


solve :: String -> IO ()
solve x = do
    let input = parseinput x
    let sorted = sort input
    let revSorted = reverse sorted

    putStr "Par 1: "
    print $ solve1 2020 sorted revSorted

    putStr "Part 2: "
    print $ firstJust (\target -> (* target) <$> solve1 (2020 - target) sorted revSorted) sorted
