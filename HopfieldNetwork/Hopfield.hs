module Hopfield (
  Spin
, Index
, LearningRate
, State
, fromBits
, toBits
, len
, Hopfield
, emptyHopfield
, weight
, update
, memorize
) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

type Spin = Double
type Index = Int
type LearningRate = Double

--- State ---

newtype State = State { getSeq :: Seq.Seq Spin } deriving Eq

fromList :: [Spin] -> State
fromList = State . Seq.fromList

toList :: State -> [Spin]
toList = Fold.toList . getSeq

fromBits :: String -> State
fromBits bits = fromList $ bitToNum <$> bits
    where bitToNum x | x == head "1" = 1.0
                     | otherwise = -1.0

toBits :: State -> String
toBits state = concat $ numToBit <$> toList state
    where numToBit x | x > 0 = "1"
                     | otherwise = "0"

instance Show State where
    show state = show $ toBits state

len :: State -> Int
len = length . toList

enum :: State -> [(Int, Spin)]
enum state = zip [1, 2..] spins
    where spins = toList state

updateState :: Index -> Spin -> State -> State
updateState i spin state = State $ Seq.update i spin (getSeq state)

--- Hopfield ---

newtype Hopfield = Hopfield { getWeightMap :: Map.Map (Int, Int) Double }

instance Show Hopfield where
    show (Hopfield weightMap) = show weightMap

emptyHopfield :: Hopfield
emptyHopfield = Hopfield emptyWeightMap
    where emptyWeightMap = Map.fromList []

weight :: Hopfield -> Index -> Index -> Double
weight hopfield i j = nothingToZero $ Map.lookup (i, j) $ getWeightMap hopfield
    where nothingToZero Nothing = 0
          nothingToZero (Just x) = x

activity :: Double -> Double
activity x
    | x >= 0 = 1
    | otherwise = -1

update :: Hopfield -> State -> Index -> State
update hopfield state i = updateState i (activity a) state
    where w = weight hopfield
          a = sum [w i j * sj | (j, sj) <- enum state]

updateWeight :: Index -> Index -> Double -> Hopfield -> Hopfield
updateWeight i j delta hopfield = Hopfield $ Map.insert (i, j) w $ getWeightMap hopfield
    where w = delta + weight hopfield i j

memorize :: LearningRate -> Hopfield -> State -> Hopfield
memorize eta hopfield state = foldr update' hopfield deltaIJ
    where update' (i, j, delta) = updateWeight i j delta
          deltaIJ = do
            (i, u) <- enum state
            (j, v) <- enum state
            if i == j
                then return (i, j, 0)
            else
                return (i, j, eta * u * v)
