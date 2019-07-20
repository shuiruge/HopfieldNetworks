module Index
( Index (..)
) where

newtype Index = Index { components :: [Int] } deriving (Eq, Ord, Show)