module Index
( Index (..)
) where

newtype Index = Index { components :: [Int] } deriving (Eq, Ord)

instance Show Index where
  show = show . components