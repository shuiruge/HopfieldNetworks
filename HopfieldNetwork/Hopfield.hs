module Hopfield (
  LearningRate
, Hopfield
, weightMap
, emptyHopfield
, weight
, initialize
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

type LearningRate = Float
type Weight = Float

-- The weight at position '(i, j)' where i <= j shall be kept absent.
newtype Hopfield = Hopfield { weightMap :: Map.Map (Index, Index) Weight }

-- TODO: Finize the printing
instance Show Hopfield where
    show hopfield = show $ weightMap hopfield

emptyHopfield :: Hopfield
emptyHopfield = Hopfield emptyWeightMap
    where emptyWeightMap = Map.fromList []

-- | The weight at position '(i, j)' or '(j, i)' of the Hopfield network.
-- | If the weight is absent at the position '(i, j)' or '(j, i)',
-- | then returns zero.
weight :: Hopfield -> Index -> Index -> Weight
weight hopfield i j
    | i < j = weight hopfield j i
    | otherwise = nothingToZero $ Map.lookup (i, j) (weightMap hopfield)
    where nothingToZero Nothing = 0
          nothingToZero (Just x) = x

-- Initialize the weight at position '(i, j)'
initialize :: Index -> Index -> Hopfield -> Hopfield
initialize i j
    | i <= j = id
    | otherwise = Hopfield . Map.insert (i, j) 0 . weightMap

-- The activity rule of Hopfield network
activity :: (Real a) => a -> Spin
activity x = if x >= 0 then Up else Down

-- | Auxillary function for `asynUpdate`
-- | The update rule of Hopfield network for one index of the state
update :: Hopfield -> Index -> State -> State
update hopfield i state = updateState i (activity a) state
    where w = weight hopfield
          a = sum [w i j * toReal sj | (j, sj) <- toList state]

-- The asynchronous update rule of Hopfield network
asynUpdate :: Hopfield -> [Index] -> State -> State
asynUpdate hopfield indexList state = foldr (update hopfield) state indexList

-- The asynchronous update rule of Hopfield network with ordinal indices
ordinalAsynUpdate :: Hopfield -> State -> State
ordinalAsynUpdate hopfield state = asynUpdate hopfield (getIndexList state) state

-- The asynchronous update rule of Hopfield network with random indices
randomAsynUpdate :: Hopfield -> State -> IO State
randomAsynUpdate hopfield state = do
    let indexList = getIndexList state
    randomIndexList <- shuffle indexList
    return $ asynUpdate hopfield randomIndexList state

-- | Auxillary function for 'learn'
-- | Add 'deltaW' to the weight at position '(i, j)'. If the position is absent,
-- | then returns the origin.
updateWeight :: Index -> Index -> Weight -> Hopfield -> Hopfield
updateWeight i j deltaW (Hopfield w) = Hopfield $ Map.adjust (+ deltaW) (i, j) w

-- The $dW_{ij} / dt$ in plasticity learning
type LearningRule = Hopfield -> State -> [(Index, Index, Weight)]

hebbRule :: LearningRule
hebbRule _ state = do
    (i, u') <- toList state
    (j, v') <- toList state
    let u = toReal u'
        v = toReal v'
        dW | i == j = 0
           | otherwise = u * v
    return (i, j, dW)

-- | Notice that the Oja's rule herein is symmetric, unlike the Oja's rule
-- | represented otherwhere on the net
-- | TODO: Add the proof of boundness of the weight by this Oja's rule
ojaRule :: Weight -> LearningRule
ojaRule r hopfield state = do
    (i, u') <- toList state
    (j, v') <- toList state
    let u = toReal u'
        v = toReal v'
        w = weight hopfield i j
        dW | i == j = 0
           | otherwise = r**2 * u * v - 0.5 * (u**2 + v**2) * w
    return (i, j, dW)

-- Memorizes the state into the Hopfield network
learn :: LearningRule -> LearningRate -> State -> Hopfield -> Hopfield
learn rule eta state hopfield = foldl update' hopfield dWij
    where update' hopfield' (i, j, dW)
            | i > j = updateWeight i j (eta * dW) hopfield'
            | otherwise = hopfield'
          dWij = rule hopfield state

-- Resets the $W_ij$ of the Hopfield network
resetWeight :: Index -> Index -> Weight -> Hopfield -> Hopfield
resetWeight i j newW hopfield = Hopfield $ Map.insert (i, j) newW wMap 
    where wMap = weightMap hopfield
