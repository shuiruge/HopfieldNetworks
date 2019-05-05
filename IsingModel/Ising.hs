-- | Implements the Ising model, including spin (data Spin), the state of
-- | lattices (data State), the Ising model (data IsingModel), and the
-- | energy (function getEnergy) of the Ising model.

module Ising
( Spin
, State
, getState
, IsingModel
, getEnergy
) where

import qualified Data.Map as M


------------ Spin -------------

data Spin = Up | Down deriving Eq

instance Show Spin where
    show Up = "1"
    show Down = "0"

getSpin :: (Num a, Ord a) => a -> Spin
getSpin x | x > 0 = Up
          | otherwise = Down

spinToFloat :: Spin -> Float
spinToFloat Up = 1
spinToFloat Down = -1


------------ State -------------

data State = State { spinList :: [Spin] }

instance Show State where
    show state = foldl (\x y -> x ++ show y) "" (spinList state)

getState :: (Num a, Ord a) => [a] -> State
getState xs = State (getSpin <$> xs)

floatList :: State -> [Float]
floatList state = spinToFloat <$> (spinList state)


------------------ Model ------------------

data IsingModel = IsingModel { j :: M.Map (Integer, Integer) Float
                             , h :: M.Map Integer Float }

nothingToZero :: (Num a) => Maybe a -> a
nothingToZero Nothing = 0
nothingToZero (Just x) = x

getJ :: IsingModel -> Integer -> Integer -> Float
getJ (IsingModel j h) m n = nothingToZero $ M.lookup (m, n) j

getH :: IsingModel -> Integer -> Float
getH (IsingModel j h) m = nothingToZero $ M.lookup m h


------------------ Energy ------------------

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [1, 2..]

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

getEnergy :: IsingModel -> State -> Float
getEnergy model state = -1 * (sum' hs + sum' js)
    where -- [x_m], in the form [(m, x)] where x is eigher +1 or -1
          xs = enumerate (floatList state)
          -- [ H_m * x_m ]
          hs = (\(m, x) -> x * getH model m) <$> xs
          -- [ J_mn * x_m * x_n ]
          js = (\(m, x) (n, y) -> x * y * (getJ model m n)) <$> xs <*> xs