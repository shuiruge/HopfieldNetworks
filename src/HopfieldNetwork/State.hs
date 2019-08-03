module HopfieldNetwork.State
( State
, indices
, spins
, state
, toList
, fromBits
, updateState
) where

import qualified Data.Map.Lazy as Map
import Data.List (foldl')
import HopfieldNetwork.Index
import HopfieldNetwork.Spin

newtype State = State { indexSpinMap :: Map.Map Index Spin } deriving Eq

state :: [(Index, Spin)] -> State
state = State . Map.fromList

-- | With index starting at one.
fromBits :: String -> State
fromBits bits =
  let
    fromBit' (i, bit) = (i, spin bit)
    enumerate = zip [index [i] | i <- [1..]]
  in
    state $ map fromBit' (enumerate bits)

toList :: State -> [(Index, Spin)]
toList = Map.toList . indexSpinMap

indices :: State -> [Index]
indices = Map.keys . indexSpinMap

spins :: State -> [Spin]
spins = Map.elems . indexSpinMap

getSpin :: State -> Index -> Maybe Spin
getSpin stat i = Map.lookup i (indexSpinMap stat)

toBits :: State -> String
toBits stat =
  let
    toStr :: Spin -> String
    toStr x = if x >= 0 then "1" else "0"

  in
    foldl' (++) "" (map toStr (spins stat))

instance Show State where
  show = toBits

-- | Updates the state by replacing the spin at the index.
updateState :: Index -> Spin -> State -> State
updateState i spin = State . Map.adjust (const spin) i . indexSpinMap