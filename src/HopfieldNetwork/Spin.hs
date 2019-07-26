module Spin (Spin, spin) where

type Spin = Double

spin :: (Show a) => a -> Spin
spin x = let s = show x
         in if s == "1" || s == "'1'" then 1 else -1