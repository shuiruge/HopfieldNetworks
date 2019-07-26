module Hopfield
( Weight
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

import qualified Data.Map.Lazy as Map
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
    nothingToZero x = case x of Nothing -> 0
                                Just x -> x
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
energy hopf stat =
  let
    s = toList stat
    w = weight hopf
  in
    - 0.5 * sum [si * w i j * sj | (i, si) <- s, (j, sj) <- s]


------------ Update State --------------


-- | The asynchronous update rule of Hopfield network
asynUpdate :: Hopfield -> [Index] -> State -> State
asynUpdate hopf indexList stat =
  let
    -- | The activity rule of Hopfield network
    activity :: (Real a) => a -> Spin
    activity x = if x >= 0 then 1 else -1

    -- | The update rule of Hopfield network for one index of the state
    update :: Hopfield -> Index -> State -> State
    update h i s = 
      let
        w = weight h
        a = sum [w i j * sj | (j, sj) <- toList s]
      in
        updateState i (activity a) s

  in foldr' (update hopf) stat indexList


------------ Learning --------------

{-
  The general form of Hebbian learning rule is, by locality,

    $$ \frac{ dw_{ij} }{ dt } = f(w_{ij}, x_i, x_j) $$

  where, by symmetry, $f(w, x, y) \equiv f(w, y, x)$.
-}
type LearningRule = (Weight -> Spin -> Spin -> Weight)  -- f(w, x, y)


{-
  The proof that the hebb rule makes the reference states the memeory of
  the network can be found in Mackay's book, section 42.7.
-}
hebbRule :: LearningRule
hebbRule = \w x y -> x * y


{-
  Oja's rule herein is

    $$ \frac{ dw_{ij} }{ dt } = R^2 x_i x_j - w_ij, $$

  wherein the $R$ and $t$ both have dimension $1$, $x$ has $L$, and $w$ has $L^2$.

  Notice that the Oja's rule herein is symmetric, and having correct dimension,
  unlike the Oja's rule represented otherwhere on the net.

  It seems that the ojaRule cannot gives the correct result if the learning
  rate is large (say 1), even for a large epochs of learning.

  For all $(i, j)$, the weight $W_{ij}$ by this Oja's rule is bounded between
  $-1$ and $+1$.

  TODO: Add the proof that the updating of state does converge to the reference
        state learned by Oja's rule.
-}
ojaRule :: Weight -> LearningRule
ojaRule r = \w x y -> r**2 * x * y - w


type LearningRate = Double

-- | Memorizes the state into the Hopfield network
learn :: LearningRule -> LearningRate -> State -> Hopfield -> Hopfield
learn rule eta stat hopf =
  let
    -- Add 'dW' to the weight at position '(i, j)'. If the position is absent,
    -- then returns the origin.
    updateWeight :: Index -> Index -> Weight -> Hopfield -> Hopfield
    updateWeight i j dW = Hopfield . Map.adjust (+ dW) (i, j) . weightMap

    update' (i, j, dW) = updateWeight i j (eta * dW)

    dWij = do
      (i, x) <- toList stat
      (j, y) <- toList stat
      let
        w = weight hopf i j
        dW = if i == j then 0 else rule w x y
      return (i, j, dW)
  in
    foldr' update' hopf dWij