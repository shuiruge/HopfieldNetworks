module Spin (
  Spin (..)
, toFloat
, fromBit
) where

data Spin = Up | Down deriving Eq
instance Show Spin where
  show Up = "1"
  show Down = "0"

toFloat :: Spin -> Float
toFloat Up = 1
toFloat Down = -1

fromBit :: Char -> Spin
fromBit '1' = Up
fromBit _ = Down