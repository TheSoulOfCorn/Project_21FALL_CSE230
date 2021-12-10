{-# LANGUAGE RecordWildCards #-}
module Model.Score 
  ( -- * Types
    Score

    -- * Energy API
  , init
  , add
  , minus
  , setZero
  , setMaxScore
  , getCurrScore
  , getMaxScore
  )
  where
import Prelude hiding (init)
-------------------------------------------------------------------------------
-- | Score --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Score = Score 
  { scFarmer :: Int,
    scMax    :: Int   }  -- ^ total number of boards
  deriving (Eq, Ord, Show)

init :: Int -> Score    -- init score to zero.
init n = Score n n 

add :: Score -> Score
add sc = sc { scFarmer = scFarmer sc + 1 }

minus :: Score -> Score
minus sc = sc { scFarmer = scFarmer sc - 1 }

setZero :: Score -> Score
setZero sc = sc { scFarmer = 0 }

setMaxScore :: Score -> Score
setMaxScore sc = sc { scMax = if (curr > max) then curr else max}
  where curr = getCurrScore sc
        max = getMaxScore sc

getCurrScore :: Score -> Int
getCurrScore Score {..} = scFarmer

getMaxScore :: Score -> Int
getMaxScore Score {..} = scMax