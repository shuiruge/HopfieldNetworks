module State
( State
, getSpin
, getIndexList
, fromList
, toList
, fromBits
, updateState
) where

import qualified Data.Map as Map
import Index
import Spin

newtype State = State { indexSpinMap :: Map.Map Index Spin } deriving Eq

-- Also add the '(Zero, Up)' to state.
fromList :: [(Index, Spin)] -> State
fromList list = State $ Map.fromList nonZeroList
    where nonZeroList = filter (\(index, _) -> index /= Zero) list

-- | With index starting at one.
fromBits :: String -> State
fromBits bits = fromList $ map fromBit' (enumerate bits)
    where fromBit' (index, bit) = (index, fromBit bit)
          enumerate = zip [Index [i] | i <- [1..]]

toList :: State -> [(Index, Spin)]
toList = Map.toList . indexSpinMap

getIndexList :: State -> [Index]
getIndexList = Map.keys . indexSpinMap

getSpin :: State -> Index -> Maybe Spin
getSpin state i = Map.lookup i (indexSpinMap state)

instance Show State where
    show state = concat $ showMaybeSpin . getSpin state <$> indexList
        where indexList = getIndexList state
              showMaybeSpin Nothing = "X"
              showMaybeSpin (Just spin) = show spin

-- | Updates the state by replacing the spin at the index.
updateState :: Index -> Spin -> State -> State
updateState i spin state = State $ Map.adjust (\_ -> spin) i (indexSpinMap state)
