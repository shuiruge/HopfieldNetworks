module Hopfield (
  Weight
, Hopfield
, weightMap
, emptyHopfield
, connectWith
, connect
, weight
, connections
, setWeight
, weightNorm
, energy
, asynUpdate
, LearningRate
, LearningRule
, hebbRule
, ojaRule
, learn
) where

import qualified Data.Map.Strict as Map
import Index
import Spin
import State
import Util (foldr')


------------ Constuction --------------

type Weight = Double

-- The weight at position '(i, j)' where i >= j shall be kept absent.
newtype Hopfield = Hopfield { weightMap :: Map.Map (Index, Index) Weight }

-- TODO: Finize the printing
instance Show Hopfield where
  show hopfield = show $ weightMap hopfield

emptyHopfield :: Hopfield
emptyHopfield = Hopfield $ Map.fromList []

-- Add connection between indices 'i' and 'j' on the Hopfield network.
connectWith :: Weight -> Index -> Index -> Hopfield -> Hopfield
connectWith w i j
  | i >= j = id
  | otherwise = Hopfield . Map.insert (i, j) w . weightMap

-- Add connection between indices 'i' and 'j' on the Hopfield network.
connect :: Index -> Index -> Hopfield -> Hopfield
connect = connectWith 0


------------ Deconstruction --------------

-- | The weight at position '(i, j)' or '(j, i)' of the Hopfield network.
-- | If the weight is absent at the position '(i, j)' or '(j, i)',
-- | then returns zero.
weight :: Hopfield -> Index -> Index -> Weight
weight hopfield i j =
  let
    nothingToZero :: Maybe Weight -> Weight
    nothingToZero Nothing = 0
    nothingToZero (Just x) = x
  in
    if i > j
      then weight hopfield j i
    else
      nothingToZero $ Map.lookup (i, j)
                    $ weightMap hopfield

-- | All the connections within the Hopfield network.
connections :: Hopfield -> [(Index, Index)]
connections = Map.keys . weightMap

-- | Manually set the $W_ij$ of the Hopfield network
-- | If the weight at position '(i, j)' does not exist, then returns the origin.
setWeight :: Index -> Index -> Weight -> Hopfield -> Hopfield
setWeight i j w = Hopfield . Map.adjust (const w) (i, j) . weightMap


------------ Util Functions --------------

-- | The p-norm of the weight-matrix of the Hopfield network.
weightNorm :: Weight -> Hopfield -> Weight
weightNorm p hopfield =
  let
    norm :: [Weight] -> Weight
    norm xs = sum (map (**p) xs) ** (1/p)
  in 
    norm [v | (_, v) <- (Map.toList . weightMap) hopfield]

energy :: Hopfield -> State -> Double
energy hopfield state =
  let
    toDouble' :: Maybe Spin -> Double
    toDouble' x = case x of
      Nothing -> 0
      (Just spin) -> toDouble spin

    s = toDouble' . getSpin state
    w = weight hopfield
    ids = getIndexList state
  in
    - 0.5 * sum [s i * w i j * s j | i <- ids, j <- ids]


------------ Update State --------------

-- | The activity rule of Hopfield network
activity :: (Real a) => a -> Spin
activity x
  | x >= 0 = Up
  | otherwise = Down

-- | The asynchronous update rule of Hopfield network
asynUpdate :: Hopfield -> [Index] -> State -> State
asynUpdate hopfield indexList state =
  let
    -- The update rule of Hopfield network for one index of the state
    update :: Hopfield -> Index -> State -> State
    update h i s = 
      let
        w = weight h
        a = sum [w i j * toDouble sj | (j, sj) <- toList s]
      in
        updateState i (activity a) s
  in
    foldr' (update hopfield) state indexList


------------ Learning --------------

-- | The $dW_{ij} / dt$ in plasticity learning
type LearningRule = Hopfield -> State -> [(Index, Index, Weight)]

{-
  The proof that the hebb rule makes the reference states the memeory of
  the network can be found in Mackay's book, section 42.7.
-}
hebbRule :: LearningRule
hebbRule _ state = do
  (i, u') <- toList state
  (j, v') <- toList state
  let
    u = toDouble u'
    v = toDouble v'
    dW
      | i == j = 0
      | otherwise = u * v
  return (i, j, dW)

{-
  Oja's rule herein is

    $$ \frac{ dw_{ij} }{ dt } = R^2 x_i x_j - w_ij, $$

  wherein the $R$ and $t$ both have dimension $1$, $x$ has $L$, and $w$ has $L^2$.

  Notice that the Oja's rule herein is symmetric, and having correct dimension,
  unlike the Oja's rule represented otherwhere on the net.

  It seems that the ojaRule cannot gives the correct result if the learning
  rate is large (say 1), even for a large epochs of learning.

  TODO:
    1. Add the proof of boundness of the weight by this Oja's rule.
    2. Add the proof that the updating of state does converge to the reference
       state learned by Oja's rule.
-}
ojaRule :: Weight -> LearningRule
ojaRule r hopfield state = do
  (i, u') <- toList state
  (j, v') <- toList state
  let
    u = toDouble u'
    v = toDouble v'
    w = weight hopfield i j
    dW
      | i == j = 0
      | otherwise = r**2 * u * v - w
  return (i, j, dW)

type LearningRate = Double

-- | Memorizes the state into the Hopfield network
learn :: LearningRule -> LearningRate -> State -> Hopfield -> Hopfield
learn rule eta state hopfield =
  let
    -- Add 'dW' to the weight at position '(i, j)'. If the position is absent,
    -- then returns the origin.
    updateWeight :: Index -> Index -> Weight -> Hopfield -> Hopfield
    updateWeight i j dW = Hopfield . Map.adjust (+ dW) (i, j) . weightMap

    update' (i, j, dW) = updateWeight i j (eta * dW)
    dWij = rule hopfield state
  in
    foldr' update' hopfield dWij