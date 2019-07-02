module State (
  Index
, State
, getSpin
, getIndexList
, fromList
, toList
, fromBits
, updateState
) where

import qualified Data.Map as Map
import Spin

type Index = Int

newtype State = State { getIndexSpinMap :: Map.Map Index Spin } deriving Eq

fromList :: [(Index, Spin)] -> State
fromList = State . Map.fromList

-- | With index starting at zero
fromBits :: String -> State
fromBits bits = fromList $ map fromBit' (enumerate bits)
    where fromBit' (index, bit) = (index, fromBit bit)
          enumerate = zip [0, 1..]

toList :: State -> [(Index, Spin)]
toList = Map.toList . getIndexSpinMap

getIndexList :: State -> [Index]
getIndexList = Map.keys . getIndexSpinMap

getSpin :: State -> Index -> Maybe Spin
getSpin state index = Map.lookup index (getIndexSpinMap state)

instance Show State where
    show state = concat $ showMaybeSpin . getSpin state <$> suppliedIndexList
        where indexList = getIndexList state
              suppliedIndexList = [(head indexList)..(last indexList)]
              showMaybeSpin Nothing = "X"
              showMaybeSpin (Just spin) = show spin

-- | Updates the state by replacing the spin at the index
-- | If the state has no value at the index, insert the value onto the index
updateState :: State -> Index -> Spin -> State
updateState state i spin = State $ Map.insert i spin (getIndexSpinMap state)