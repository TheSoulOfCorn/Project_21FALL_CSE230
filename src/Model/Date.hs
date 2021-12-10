{-# LANGUAGE RecordWildCards #-}
module Model.Date
  ( -- * Types
    Date

    -- * Energy API
  , init
  , resetDate
  , getDate
  , dateDown
  )
  where
import Prelude hiding (init)

data Date = Date 
  { dFarmer :: Int  -- ^ total number of boards
  , dMin    :: Int  -- ^ points for player O 
  }
  deriving (Eq, Ord, Show)

init :: Int -> Date
init n = Date n 0 

resetDate :: Int -> Date -> Date
resetDate num d =  d {dFarmer = num}

getDate :: Date -> Int
getDate Date {..} =  dFarmer

dateDown :: Date -> Date
dateDown d = if dFarmer d - 1 > 0 then d { dFarmer = dFarmer d - 1 } else d { dFarmer = 0 }