module Util
( shuffle
, foldr'
, foldM'
, duplicate
, mean
, var
, std
) where


import System.Random
import Data.Array.IO
import Control.Monad
import Data.List (foldl')


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


-- | Right-associative fold of a structure but with strict application of the operator.
foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr' f = foldl' (flip f)


-- | Strict version of 'foldM'
-- C.f. https://stackoverflow.com/a/8919106
foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs


-- | Makes, say, [1, 2] to [1, 2, 1, 2, 1, 2] if 'n' is 3.
duplicate :: Int -> [a] -> [a]
duplicate n xs = foldr' (++) [] $ replicate n xs


mean :: [Double] -> Double
mean xs =
  let n = fromIntegral $ length xs
  in sum xs / n

var :: [Double] -> Double
var xs =
  let
    xBar = mean xs
    square x = x * x
  in mean $ square . (xBar -) <$> xs

std :: [Double] -> Double
std = sqrt . var