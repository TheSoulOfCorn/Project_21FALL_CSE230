{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Score  as Score
import qualified Model.Player as Player
import qualified Model.Energy as Energy
import qualified Model.Date   as Date
-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  { psFarmer :: Player.Player   -- ^ player X info
  , psEnergy :: Energy.Energy
  , psScore  :: Score.Score     -- ^ current score
  , psBoard  :: Board.Board     -- ^ current board
  , psDate   :: Date.Date
  , psPos    :: Board.Pos       -- ^ current cursor
  , psResult :: Board.Result () -- ^ result 
  , psEnd    :: Bool    
  } 

init :: Int -> PlayState
init n = PS 
  { psFarmer = Player.human
  , psEnergy = Energy.init 100
  , psScore  = Score.init 0
  , psBoard  = Board.init
  , psDate   = Date.init 1
  , psPos    = head Board.positions 
  , psResult = Board.Cont ()
  , psEnd    = False
  }

isCurr :: PlayState -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos s 

next :: PlayState -> Board.Result Board.Board -> Either (Board.Result ()) PlayState
next s (Board.Cont b') = Right (s { psBoard = b'})

-- nextBoard :: PlayState -> Board.Result a -> Either (Board.Result ()) PlayState
-- nextBoard s res = case res' of
--                     _           -> Right s' 
--   where 
--     sc'  = Score.add (psScore s) (Board.boardWinner res) 
--     s'   = s { psScore = sc'                   -- update the score
--              , psBoard = mempty                -- clear the board
--              , psTurn  = Score.startPlayer sc' -- toggle start player
--              } 

