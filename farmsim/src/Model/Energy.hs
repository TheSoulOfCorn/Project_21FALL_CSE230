{-# LANGUAGE RecordWildCards #-}
module Model.Energy
  ( -- * Types
    Energy

    -- * Energy API
  , init
  , getEnergy
  , energyUp
  , energyDown
  )
  where
import Prelude hiding (init)

data Energy = Energy 
  { eFarmer :: Int  -- ^ total number of boards
  , eMax    :: Int  -- ^ points for player X 
  , eMin    :: Int  -- ^ points for player O 
  }
  deriving (Eq, Ord, Show)

init :: Int -> Energy
init n = Energy n 100 0 

getEnergy :: Energy -> Int
getEnergy Energy {..} =  eFarmer

energyUp :: Energy -> Int -> Energy
energyUp e eUp = 
    if eFarmer e + eUp > eMax e 
        then e { eFarmer = eMax e }
        else e { eFarmer = eFarmer e + eUp }

energyDown :: Energy -> Int -> Energy
energyDown e eDown = 
    if eFarmer e - eDown < eMin e 
        then e { eFarmer = eMin e }
        else e { eFarmer = eFarmer e - eDown }