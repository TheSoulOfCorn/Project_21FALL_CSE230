module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Model.Energy
import Model.Score
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player
import Model.Date
-- import Model.Player 

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  --T.VtyEvent (V.EvKey V.KEnter _) -> nextS s =<< liftIO (farm s)
  T.VtyEvent (V.EvKey (V.KChar 'p') _) -> nextSow s   =<< liftIO (farmSowA s)
  T.VtyEvent (V.EvKey (V.KChar 'o') _) -> nextSow s   =<< liftIO (farmSowR s)
  T.VtyEvent (V.EvKey (V.KChar 'w') _) -> nextWater s =<< liftIO (farmWater s)
  T.VtyEvent (V.EvKey (V.KChar 'f') _) -> nextFert s  =<< liftIO (farmFert s)
  T.VtyEvent (V.EvKey (V.KChar 'd') _) -> nextDebug s =<< liftIO (farmDebug s)
  T.VtyEvent (V.EvKey (V.KChar 'c') _) -> nextClean s  =<< liftIO (farmClean s)
  T.VtyEvent (V.EvKey (V.KChar 'r') _) -> nextReap s  =<< liftIO (farmReap s)
  T.VtyEvent (V.EvKey (V.KChar 'e') _) -> nextEat s  =<< liftIO (farmEat s)
  T.VtyEvent (V.EvKey V.KEnter _) -> nextSleep s  =<< liftIO (farmSleep s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move up    s)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move down  s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psPos = f (psPos s) }

-------------------------------------------------------------------------------
farmSowA   :: PlayState -> IO (Result Board)
farmSowR   :: PlayState -> IO (Result Board)
farmWater :: PlayState -> IO (Result Board)
farmFert  :: PlayState -> IO (Result Board)
farmDebug :: PlayState -> IO (Result Board)
farmReap  :: PlayState -> IO (Result Board)
farmSleep :: PlayState -> IO (Result Board)
-------------------------------------------------------------------------------
farmSowA s = 
  if (playerEnergy (psFarmer s)) == 0 then return Retry
  else putSeedA (psBoard s) (psEnergy s) <$> getPos s

farmSowR s = 
  if (playerEnergy (psFarmer s)) == 0 then return Retry
  else putSeedR (psBoard s) (psEnergy s) <$> getPos s

farmWater s = 
  if (playerEnergy (psFarmer s)) == 0 then return Retry
  else putWater (psBoard s) (psEnergy s) <$> getPos s

farmFert s = 
  if (playerEnergy (psFarmer s)) == 0 then return Retry
  else putFert (psBoard s) (psEnergy s) <$> getPos s

farmDebug s = 
  if (playerEnergy (psFarmer s)) == 0 then return Retry
  else putDebug (psBoard s) (psEnergy s) <$> getPos s

farmReap s = 
  if (playerEnergy (psFarmer s)) == 0 then return Retry
  else putReap (psBoard s) (psEnergy s) <$> getPos s

farmClean s = 
  if (playerEnergy (psFarmer s)) == 0 then return Retry
  else putClean (psBoard s) (psEnergy s) <$> getPos s

farmEat s = putEat (psBoard s) (psScore s) <$> getPos s
farmSleep s =  putSleep (psBoard s) (psEnergy s) <$> getPos s
    
getPos :: PlayState -> IO Pos
getPos s  = getStrategy s (psPos s) (psBoard s) 

getStrategy :: PlayState -> Strategy
getStrategy s = playerStrat (psFarmer s)

sowS :: PlayState -> PlayState
sowS s = s {psEnergy  = energyDown (psEnergy s) 20}

waterS :: PlayState -> PlayState
waterS s = s {psEnergy  = energyDown (psEnergy s) 10}

fertS :: PlayState -> PlayState
fertS s = s {psEnergy  = energyDown (psEnergy s) 10}

debugS :: PlayState -> PlayState
debugS s = s {psEnergy  = energyDown (psEnergy s) 10}

reapS :: PlayState -> PlayState
reapS s = s {psEnergy = energyDown (psEnergy s) 20,
             psScore  = add (psScore s)}

cleanS :: PlayState -> PlayState
cleanS s = s {psEnergy = energyDown (psEnergy s) 10}

eatS :: PlayState -> PlayState
eatS s = s {psEnergy = energyUp (psEnergy s) 50,
            psScore = minus (psScore s)}

sleepS :: PlayState -> PlayState
sleepS s = s {psEnergy = energyUp (psEnergy s) 100,
              psDate = dateDown (psDate s)}

endS :: PlayState -> PlayState
endS s = s {psEnd = True}
-------------------------------------------------------------------------------
nextSow :: PlayState -> Result Board -> EventM n (Next PlayState)
nextWater :: PlayState -> Result Board -> EventM n (Next PlayState)
nextDebug :: PlayState -> Result Board -> EventM n (Next PlayState)
nextFert :: PlayState -> Result Board -> EventM n (Next PlayState)
nextReap :: PlayState -> Result Board -> EventM n (Next PlayState)
nextClean :: PlayState -> Result Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextSow s b = case next s b of
  Right s' -> if (result (psBoard s) == b) then continue s' else continue (sowS s')
  Left res -> halt (s { psResult = res }) 

nextWater s b = case next s b of
  Right s' -> if (result (psBoard s) == b) then continue s' else continue (waterS s')
  Left res -> halt (s { psResult = res }) 

nextDebug s b = case next s b of
  Right s' -> if (result (psBoard s) == b) then continue s' else continue (debugS s')
  Left res -> halt (s { psResult = res }) 

nextFert s b = case next s b of
  Right s' -> if (result (psBoard s) == b) then continue s' else continue (fertS s')
  Left res -> halt (s { psResult = res }) 

nextReap s b = case next s b of
  Right s' -> if (result (psBoard s) == b) then continue s' else continue (reapS s')
  Left res -> halt (s { psResult = res }) 

nextClean s b = case next s b of
  Right s' -> if (result (psBoard s) == b) then continue s' else continue (cleanS s')
  Left res -> halt (s { psResult = res }) 

-------------------------------------------------------------------------------
nextSleep :: PlayState -> Result Board -> EventM n (Next PlayState)
nextEat :: PlayState -> Result Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------

nextSleep s b = case next s b of
  Right s' -> if ( getDate (psDate s) == 0) then continue (endS s') else continue (sleepS s')
  Left res -> halt (s { psResult = res }) 

nextEat s b = case next s b of
  Right s' -> if (getScore (psScore s) == 0) then continue s' else continue (eatS s')
  Left res -> halt (s { psResult = res })
