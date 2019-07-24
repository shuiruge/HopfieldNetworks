module State
( State
, getSpin
, getIndexList
, state
, toList
, fromBits
, updateState
) where

import qualified Data.Map.Lazy as Map
import Index
import Spin

newtype State = State { indexSpinMap :: Map.Map Index Spin } deriving Eq

state :: [(Index, Spin)] -> State
state = State . Map.fromList

-- | With index starting at one.
fromBits :: String -> State
fromBits bits =
  let
    fromBit' (i, bit) = (i, fromBit bit)
    enumerate = zip [index [i] | i <- [1..]]
  in
    state $ map fromBit' (enumerate bits)

toList :: State -> [(Index, Spin)]
toList = Map.toList . indexSpinMap

getIndexList :: State -> [Index]
getIndexList = Map.keys . indexSpinMap

getSpinList :: State -> [Spin]
getSpinList = Map.elems . indexSpinMap

getSpin :: State -> Index -> Maybe Spin
getSpin state i = Map.lookup i (indexSpinMap state)

instance Show State where
  show = concatMap show . getSpinList

-- | Updates the state by replacing the spin at the index.
updateState :: Index -> Spin -> State -> State
updateState i spin = State . Map.adjust (const spin) i . indexSpinMap