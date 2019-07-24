module Index
( Index
, index
) where

import Data.Hashable (Hashable, hash, hashWithSalt)

data Index = Index { components :: [Int] , hashValue :: Int } deriving Eq

index :: [Int] -> Index
index xs = let getHash = hash . show
           in Index xs (getHash xs)


instance Show Index where
  show = show . components

instance Ord Index where
  compare i j = compare (hashValue i) (hashValue j)

instance Hashable Index where
  hash = hashValue
  hashWithSalt s = (+ s) . hash