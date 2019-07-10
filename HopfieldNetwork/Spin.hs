module Spin (
  Spin (..)
, toReal
, fromBit
) where

data Spin = Up | Down deriving Eq
instance Show Spin where
    show Up = "1"
    show Down = "0"

toReal :: (Real a) => Spin -> a
toReal Up = 1
toReal Down = -1

fromBit :: Char -> Spin
fromBit '1' = Up
fromBit _ = Down