module HopfieldNetwork.Index
( Index
, index
) where

newtype Index = Index { components :: [Int] } deriving (Eq, Ord)

index :: [Int] -> Index
index = Index

instance Show Index where
  show = show . components
