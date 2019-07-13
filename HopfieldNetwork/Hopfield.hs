module Hopfield (
  LearningRate
, Hopfield
, weightMap
, emptyHopfield
, weight
, connect
, LearningRule
, hebbRule
, ojaRule
, asynUpdate
, ordinalAsynUpdate
, randomAsynUpdate
, learn
, resetWeight
, energy
) where

import qualified Data.Map.Strict as Map
import Index
import Spin
import State
import Util

type LearningRate = Float
type Weight = Float

-- The weight at position '(i, j)' where i >= j shall be kept absent.
newtype Hopfield = Hopfield { weightMap :: Map.Map (Index, Index) Weight }

-- TODO: Finize the printing
instance Show Hopfield where
    show hopfield = show $ weightMap hopfield

emptyHopfield :: Hopfield
emptyHopfield = Hopfield $ Map.fromList []

-- Auxillary function for 'weight'.
nothingToZero :: Maybe Weight -> Weight
nothingToZero Nothing = 0
nothingToZero (Just x) = x

-- | The weight at position '(i, j)' or '(j, i)' of the Hopfield network.
-- | If the weight is absent at the position '(i, j)' or '(j, i)',
-- | then returns zero.
weight :: Hopfield -> Index -> Index -> Weight
weight hopfield i j
    | i > j = weight hopfield j i
    | otherwise = nothingToZero
                $ Map.lookup (i, j)
                $ weightMap hopfield

-- Add connection between indices 'i' and 'j' on the Hopfield network.
connect :: Index -> Index -> Hopfield -> Hopfield
connect i j
    | i >= j = id
    | otherwise = Hopfield . Map.insert (i, j) 0 . weightMap

-- The activity rule of Hopfield network
activity :: (Real a) => a -> Spin
activity x
    | x >= 0 = Up
    | otherwise = Down

-- | Auxillary function for `asynUpdate`
-- | The update rule of Hopfield network for one index of the state
update :: Hopfield -> Index -> State -> State
update hopfield i state = updateState i (activity a) state
    where w = weight hopfield
          a = sum [w i j * toFloat sj | (j, sj) <- toList state]
            + w Zero i

-- The asynchronous update rule of Hopfield network
asynUpdate :: Hopfield -> [Index] -> State -> State
asynUpdate hopfield indexList state = foldr (update hopfield) state indexList

-- The asynchronous update rule of Hopfield network with ordinal indices
ordinalAsynUpdate :: Hopfield -> State -> State
ordinalAsynUpdate hopfield state = asynUpdate hopfield (getIndexList state) state

-- The asynchronous update rule of Hopfield network with random indices
randomAsynUpdate :: Hopfield -> State -> IO State
randomAsynUpdate hopfield state = do
    randomIndexList <- shuffle $ getIndexList state
    return $ asynUpdate hopfield randomIndexList state

-- | Auxillary function for 'learn'
-- | Add 'deltaW' to the weight at position '(i, j)'. If the position is absent,
-- | then returns the origin.
updateWeight :: Index -> Index -> Weight -> Hopfield -> Hopfield
updateWeight i j dW = Hopfield . Map.adjust (+ dW) (i, j) . weightMap

-- The $dW_{ij} / dt$ in plasticity learning
type LearningRule = Hopfield -> State -> [(Index, Index, Weight)]

-- For introducing bias in the computation.
addZero :: [(Index, Spin)] -> [(Index, Spin)]
addZero = (:) (Zero, Up)

hebbRule :: LearningRule
hebbRule _ state = do
    (i, u') <- (addZero . toList) state
    (j, v') <- (addZero . toList) state
    let
        u = toFloat u'
        v = toFloat v'
        dW | i == j = 0
           | otherwise = u * v
    return (i, j, dW)

-- | Notice that the Oja's rule herein is symmetric, unlike the Oja's rule
-- | represented otherwhere on the net
-- | TODO: Add the proof of boundness of the weight by this Oja's rule
ojaRule :: Weight -> LearningRule
ojaRule r hopfield state = do
    (i, u') <- (addZero . toList) state
    (j, v') <- (addZero . toList) state
    let
        u = toFloat u'
        v = toFloat v'
        w = weight hopfield i j
        dW | i == j = 0
           | otherwise = r**2 * u * v
                       - 0.5 * (u**2 + v**2) * w
    return (i, j, dW)

-- Memorizes the state into the Hopfield network
learn :: LearningRule -> LearningRate -> State -> Hopfield -> Hopfield
learn rule eta state hopfield =
    let
        update' (i, j, dW) = updateWeight i j (eta * dW)
        dWij = rule hopfield state
    in
        foldr update' hopfield dWij

-- Resets the $W_ij$ of the Hopfield network
resetWeight :: Index -> Index -> Weight -> Hopfield -> Hopfield
resetWeight i j newW = Hopfield . Map.adjust (const newW) (i, j) . weightMap

-- Auxillary function of 'energy'.
toFloat' :: Maybe Spin -> Float
toFloat' Nothing = 0
toFloat' (Just spin) = toFloat spin

energy :: Hopfield -> State -> Float
energy hopfield state =
    let
        s = toFloat' . getSpin state
        w = weight hopfield
        ids = getIndexList state
    in
        - 0.5 * sum [s i * w i j * s j | i <- ids, j <- ids]
        - sum [s i * w Zero i | i <- ids]  -- bias part