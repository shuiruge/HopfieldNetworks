module Util
( shuffle
, duplicate
) where

import System.Random
import Data.Array.IO
import Control.Monad

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray' n xs
  forM [1..n] $ \i -> do
    j <- randomRIO (i,n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray' :: Int -> [a] -> IO (IOArray Int a)
    newArray' m =  newListArray (1, m)

-- Makes, say, [1, 2] to [1, 2, 1, 2, 1, 2] if 'n' is 3.
duplicate :: Int -> [a] -> [a]
duplicate n xs = foldr (++) [] $ take n (repeat xs)