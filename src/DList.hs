module DList where

import Control.Monad.State.Lazy
import Debug.Trace
import Util(chunkList)

data DListState a = DListState { cidx  :: Int
                               , chunksize :: Int
                               , current :: [a]
                               , front :: [[a]]
                               , back  :: [[a]]
                               } deriving (Show)

type DList a = State (DListState a)

runDList2d :: DList (DListState a) b -> [[a]] -> (b, DListState (DListState a))
runDList2d l a = runState l $ fromList 3 $ map (fromList 3) a


runDList :: DList a b -> [a] -> b
runDList l a = x
    where (x, _) = runState l $ fromList 3 a

runDList' :: DList a b -> [a] -> (b, DListState a)
runDList' l a = runState l $ fromList 3 a

fromList :: Int -> [a] -> DListState a
fromList chunksize l = DListState 0 chunksize c [] cs
    where (c:cs) = chunkList chunksize l


index' :: Show a => Int -> DList a a
index' i = do
    s <- get
    let (o, s') = index'' s i
    put s'
    return o


foldWith' :: (c -> DList a d) -> (d -> b -> b) -> b -> [c] -> DList a b
foldWith' _ _ b [] = return b
foldWith' g f b (c:cs) = do
    a <- g c
    foldWith' g f (f a b) cs


fold' :: Show a => (a -> b -> b) -> b -> [Int] -> DList a b
fold' = foldWith' index'


index'' :: Show a => DListState a -> Int -> (a, DListState a)
index'' s@DListState{cidx=cidx, chunksize=cs, current=c, front=front, back=back} i
    | i - cidx >= cs = index'' (DListState (cidx+cs) cs b (c:f:fs) bs) i
    | i - cidx < 0   = index'' (DListState (cidx-cs) cs f fs (c:b:bs)) i
    | otherwise      = (c !! (i-cidx), s)
    where
        (f:fs) = front
        (b:bs) = back


map' :: Show a => [Int] -> DList a [a]
map' = fold' (:) []


update' :: Show a => (a -> (b, a)) -> Int -> DList a b
update' f i = do
                a <- index' i
                let (o, a') = f a
                modify $ bb i a'
                return o
    where bb i b s@DListState{current=c, cidx=cidx} = s{current=setAt (i-cidx) b c}


get2d' :: Show a => (Int, Int) -> DList (DListState a) a
get2d' (x, y) = update' (`index''` y) x

fold2d' :: Show a => (a -> b -> b) -> b -> [(Int, Int)] -> DList (DListState a) b
fold2d' = foldWith' get2d'

map2d' :: Show a => [(Int, Int)] -> DList (DListState a) [a]
map2d' = fold2d' (:) []


setAt :: Int -> a -> [a] -> [a]
setAt 0 a (_:xs) = a:xs
setAt i a (x:xs) = x:setAt (i-1) a xs
