{-# LANGUAGE RecordWildCards #-}
module Model.Score 
  ( -- * Types
    Score

    -- * Energy API
  , init
  , add
  , minus
  , getScore
  )
  where
import Prelude hiding (init)
-------------------------------------------------------------------------------
-- | Score --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Score = Score 
  { scFarmer  :: Int }  -- ^ total number of boards
  deriving (Eq, Ord, Show)

init :: Int -> Score    -- init score to zero.
init n = Score n 

add :: Score -> Score
add sc = sc { scFarmer = scFarmer sc + 1 }

minus :: Score -> Score
minus sc = sc { scFarmer = scFarmer sc - 1 }

getScore :: Score -> Int
getScore Score {..} = scFarmer