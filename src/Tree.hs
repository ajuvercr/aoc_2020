module Tree where

data Tree k a = Node { item :: a
                     , key  :: k
                     , left :: Tree k a
                     , right :: Tree k a
                     , prev :: Tree k a
                     } | None deriving (Show)

empty :: Ord k => Tree k a
empty = None

insertAt :: Ord k => Tree k a -> k -> a -> Tree k a
insertAt None key value = Node value key None None None
insertAt t k v
    | k == key t = t {item = v}
    | k >  key t = t {left=insertAt (left t) k v}
    | otherwise  = t {right=insertAt (left t) k v}
