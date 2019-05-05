module Metropolis (
  EnergyState
, Metropolis
, update
) where

data EnergyState a = EnergyState { energy :: Float
                                 , state :: a }

data Metropolis a = Metropolis { transition :: a -> a
                               , getEnergy :: a -> Float }

update :: (Eq a) => Metropolis a -> Float -> EnergyState a -> EnergyState a
update metropolis random (EnergyState energy state)
    | exp(energy - newEnergy) > random = EnergyState newEnergy newState
    | otherwise = EnergyState energy state
    where newState = (transition metropolis) state
          newEnergy = (getEnergy metropolis) newState

-- getAcceptRatio :: [EnergyState] -> Float
-- getAcceptRatio energyStates = equalStates / allStates