module Index (
  Index (..)
) where

newtype Index = Index { components :: [Int] } deriving (Ord, Eq)

instance Show Index where
    show = show . components