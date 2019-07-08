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

newtype Index = Index [Int] deriving (Ord, Eq)

instance Show Index where
    show (Index ints) = show ints

newtype State = State { indexSpinMap :: Map.Map Index Spin } deriving Eq

fromList :: [(Index, Spin)] -> State
fromList = State . Map.fromList

-- | With index starting at zero
fromBits :: String -> State
fromBits bits = fromList $ map fromBit' (enumerate bits)
    where fromBit' (index, bit) = (index, fromBit bit)
          enumerate = zip [Index [i] | i <- [0, 1..]]

toList :: State -> [(Index, Spin)]
toList = Map.toList . indexSpinMap

getIndexList :: State -> [Index]
getIndexList = Map.keys . indexSpinMap

getSpin :: State -> Index -> Maybe Spin
getSpin state index = Map.lookup index (indexSpinMap state)

instance Show State where
    show state = concat $ showMaybeSpin . getSpin state <$> indexList
        where indexList = getIndexList state
              showMaybeSpin Nothing = "X"
              showMaybeSpin (Just spin) = show spin

-- | Updates the state by replacing the spin at the index
-- | If the state has no value at the index, insert the value onto the index
updateState :: State -> Index -> Spin -> State
updateState state i spin = State $ Map.insert i spin (indexSpinMap state)