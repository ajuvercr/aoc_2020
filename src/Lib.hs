module Lib
    ( printArray
    ) where


printArray :: Show a => [a] -> IO ()
printArray = foldMap print
