module DList where

import Control.Monad.State.Lazy
import Debug.Trace

data DListState a = DListState { cidx  :: Int
                     , front :: [a]
                     , back  :: [a]
                     } deriving (Show)

type DList a = State (DListState a)


runDList :: DList a b -> [a] -> b
runDList l a = x
    where (x, _) = runState l $ fromList a

runDList' :: DList a b -> [a] -> (b, DListState a)
runDList' l a = runState l $ fromList a

fromList :: [a] -> DListState a
fromList = DListState 0 []


index' :: Int -> DList a a
index' i = do
    s <- get
    let (o, s') = index'' s i
    put s'
    return o


fold' :: (a -> b -> b) -> b -> [Int] -> DList a b
fold' _ b [] = return b
fold' f b (i:is) = do
    a <- index' i
    fold' f (f a b) is


index'' :: DListState a -> Int -> (a, DListState a)
index'' s@DListState{cidx=cidx, front=f, back=b} i
    | i < cidx  = index'' (DListState (cidx-1) (tail f) (z f b)) i
    | i > cidx  = index'' (DListState (cidx+1) (z b f) (tail b)) i
    | otherwise = (head b, s)
    where z (x:_) xs = trace "here" x:xs


map' :: [Int] -> DList a [a]
map' = fold' (:) []


update' :: (a -> (b, a)) -> Int -> DList a b
update' f i = do
                a <- index' i
                let (o, a') = f a
                modify $ bb a'
                return o
    where bb b s@DListState{back=(_:bs)} = s{back=b:bs}
