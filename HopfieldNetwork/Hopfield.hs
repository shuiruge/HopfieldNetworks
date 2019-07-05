module Hopfield (
  LearningRate
, Hopfield
, weightMap
, emptyHopfield
, weight
, asynUpdate
, ordinalAsynUpdate
, randomAsynUpdate
, memorize
, reset
) where

import qualified Data.Map.Strict as Map
import Spin
import State
import Util (shuffle)

type LearningRate = Double
type Weight = Double

newtype Hopfield = Hopfield { weightMap :: Map.Map (Index, Index) Weight }
instance Show Hopfield where
    show (Hopfield weightMap) = show weightMap

emptyHopfield :: Hopfield
emptyHopfield = Hopfield emptyWeightMap
    where emptyWeightMap = Map.fromList []

-- $W_{ij}$ of the Hopfield network
weight :: Hopfield -> Index -> Index -> Weight
weight hopfield i j = nothingToZero $ Map.lookup (i, j) $ weightMap hopfield
    where nothingToZero Nothing = 0
          nothingToZero (Just x) = x

-- The activity rule of Hopfield network
activity :: Double -> Spin
activity x = if x >= 0 then Up else Down

-- | Auxillary function for `asynUpdate`
-- | The update rule of Hopfield network for one index of the state
update :: Hopfield -> State -> Index -> State
update hopfield state i = updateState state i (activity a)
    where w = weight hopfield
          a = sum [w i j * toNum sj | (j, sj) <- toList state]

-- The asynchronous update rule of Hopfield network
asynUpdate :: Hopfield -> State -> [Index] -> State
asynUpdate hopfield = foldl (update hopfield)

-- The asynchronous update rule of Hopfield network with ordinal indices
ordinalAsynUpdate :: Hopfield -> State -> State
ordinalAsynUpdate hopfield state = asynUpdate hopfield state (getIndexList state)

-- The asynchronous update rule of Hopfield network with random indices
randomAsynUpdate :: Hopfield -> State -> IO State
randomAsynUpdate hopfield state = do
    let indexList = getIndexList state
    randomIndexList <- shuffle indexList
    return $ asynUpdate hopfield state randomIndexList

-- Auxillary function for `memorize`
updateWeight :: Index -> Index -> Double -> Hopfield -> Hopfield
updateWeight i j deltaW hopfield = Hopfield $ Map.insert (i, j) newW wMap 
    where newW = deltaW + weight hopfield i j
          wMap = weightMap hopfield

-- Returns the $\Delta W_{ij}$ by Hebb rule
hebbRule :: LearningRate -> State -> [(Index, Index, Double)]
hebbRule eta state = do
    (i, u) <- toList state
    (j, v) <- toList state
    let deltaW = if i == j then 0 else eta * toNum u * toNum v
    return (i, j, deltaW)

-- Memorizes the state into the Hopfield network
memorize :: LearningRate -> Hopfield -> State -> Hopfield
memorize eta hopfield state = foldr update' hopfield deltaWij
    where update' (i, j, deltaW) = updateWeight i j deltaW
          deltaWij = hebbRule eta state

-- Resets the $W_ij$ of the Hopfield network
reset :: Index -> Index -> Weight -> Hopfield -> Hopfield
reset i j newW hopfield = Hopfield $ Map.insert (i, j) newW wMap 
    where wMap = weightMap hopfield
