module Hopfield (
  LearningRate
, Hopfield
, weightMap
, emptyHopfield
, weight
, LearningRule
, hebbRule
, ojaRule
, asynUpdate
, ordinalAsynUpdate
, randomAsynUpdate
, learn
, resetWeight
) where

import qualified Data.Map.Strict as Map
import Index
import Spin
import State
import Util (shuffle)

type LearningRate = Double
type Weight = Double

newtype Hopfield = Hopfield { weightMap :: Map.Map (Index, Index) Weight }

-- TODO: Finize the printing
instance Show Hopfield where
    show hopfield = show $ weightMap hopfield

emptyHopfield :: Hopfield
emptyHopfield = Hopfield emptyWeightMap
    where emptyWeightMap = Map.fromList []

-- $W_{ij}$ of the Hopfield network
weight :: Hopfield -> Index -> Index -> Weight
weight hopfield i j
    | i == j = 0
    | i < j = weight hopfield j i
    | otherwise = nothingToZero $ Map.lookup (i, j) $ weightMap hopfield 
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

-- The $dW_{ij} / dt$ in plasticity learning
type LearningRule = Hopfield -> State -> [(Index, Index, Double)]

hebbRule :: LearningRule
hebbRule _ state = do
    (i, u') <- toList state
    (j, v') <- toList state
    let u = toNum u'
        v = toNum v'
        dW | i == j = 0
           | otherwise = u * v
    return (i, j, dW)

-- | Notice that the Oja's rule herein is symmetric, unlike the Oja's rule
-- | represented otherwhere on the net
-- | TODO: Add the proof of boundness of the weight by this Oja's rule
ojaRule :: Double -> LearningRule
ojaRule r hopfield state = do
    (i, u') <- toList state
    (j, v') <- toList state
    let u = toNum u'
        v = toNum v'
        w = weight hopfield i j
        dW | i == j = 0
           | otherwise = r**2 * u * v - 0.5 * (u**2 + v**2) * w
    return (i, j, dW)

-- Memorizes the state into the Hopfield network
learn :: LearningRule -> LearningRate -> Hopfield -> State -> Hopfield
learn rule eta hopfield state = foldr update' hopfield dWij
    where update' (i, j, dW) hopfield'
            | i > j = updateWeight i j (eta * dW) hopfield'
            | otherwise = hopfield'
          dWij = rule hopfield state

-- Resets the $W_ij$ of the Hopfield network
resetWeight :: Index -> Index -> Weight -> Hopfield -> Hopfield
resetWeight i j newW hopfield = Hopfield $ Map.insert (i, j) newW wMap 
    where wMap = weightMap hopfield
