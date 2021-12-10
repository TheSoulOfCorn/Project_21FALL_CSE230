module Model.Player where

import Model.Board

-------------------------------------------------------------------------------
-- | Players and Strategies ---------------------------------------------------
-------------------------------------------------------------------------------

data Player = Player 
  { playerEnergy  :: Int
  , playerStrat   :: Strategy
  } 

type Strategy = Pos      -- ^ current cursor
              -> Board   -- ^ current board
              -> IO Pos  -- ^ next move

human :: Player 
human = Player 100 (\p _ -> return p)