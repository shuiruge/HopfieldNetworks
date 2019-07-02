module Spin (
  Spin (..)
, toNum
, fromNum
, fromBit
) where

data Spin = Up | Down deriving Eq
instance Show Spin where
    show Up = "1"
    show Down = "0"

toNum :: (Num a, Ord a) => Spin -> a
toNum Up = 1
toNum Down = -1

fromNum :: (Num a, Ord a) => a -> Spin
fromNum x
    | x > 0 = Up
    | otherwise = Down

fromBit :: Char -> Spin
fromBit '1' = Up
fromBit _ = Down