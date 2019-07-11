module Index
( Index (..)
) where

-- The 'Zero' index is for the bias, like $W_{0 i}$.
data Index = Zero | Index { components :: [Int] } deriving Eq

instance Show Index where
    show Zero = "Zero"
    show (Index comp) = show comp

instance Ord Index where
    compare Zero Zero = EQ
    compare Zero (Index _) = LT
    compare (Index _) Zero = GT
    compare (Index comp0) (Index comp1) = compare comp0 comp1