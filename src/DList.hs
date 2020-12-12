module DList where

data DList a = DList { cidx  :: Int
                     , front :: [a]
                     , back  :: [a]
                     } deriving (Show)

fromList :: [a] -> DList a
fromList xs = DList 0 xs []

index :: DList a -> Int -> (a, DList a)
index s@DList{cidx=cidx, front=(f:fs), back=(b:bs)} i
    | i < cidx  = index (DList (cidx+1) fs       (f:b:bs)) i
    | i > cidx  = index (DList (cidx-1) (b:f:fs) bs      ) i
    | otherwise = (f, s)
index s i | i == cidx s = (head $ front s, s)
