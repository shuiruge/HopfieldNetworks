module HopfieldNetwork.State
( State
, getSpin
, getIndexList
, fromList
, toList
, fromBits
, updateState
) where

import qualified Data.Map as Map
import HopfieldNetwork.Index
import HopfieldNetwork.Spin

newtype State = State { indexSpinMap :: Map.Map Index Spin } deriving Eq

fromList :: [(Index, Spin)] -> State
fromList = State . Map.fromList . filter (\(index, _) -> index /= Zero) 

-- | With index starting at one.
fromBits :: String -> State
fromBits bits =
  let
    fromBit' (index, bit) = (index, fromBit bit)
    enumerate = zip [Index [i] | i <- [1..]]
  in
    fromList $ map fromBit' (enumerate bits)

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