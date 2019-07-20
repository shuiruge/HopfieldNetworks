module Spin (
  Spin (..)
, toDouble
, fromBit
) where

data Spin = Up | Down deriving Eq
instance Show Spin where
  show Up = "1"
  show Down = "0"

toDouble :: Spin -> Double
toDouble spin = case spin of
  Up -> 1
  Down -> -1

fromBit :: Char -> Spin
fromBit '1' = Up
fromBit _ = Down