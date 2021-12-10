{-# LANGUAGE DeriveFunctor #-}
module Model.Board 
  ( -- * Types
    Board
  , Plant (..)
  , Pos (..)
  , Result (..)

    -- * Board API
  , dim
  , (!)
  , init
  , putSeedA
  , putSeedR
  , putWater
  , putFert
  , putReap
  , putDebug
  , putSleep
  , putClean
  , putConfirm
  , positions
  , putEat
  , putRestart
  , emptyPositions
  , takenPositions
  , result
    -- * Moves
  , up
  , down
  , left
  , right
  )
  where

import Prelude hiding (init)
import Model.Energy hiding (init)
import Model.Score hiding (init)
import qualified Data.Map as M 

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

type Board = M.Map Pos Plant

data Plant
  = SeedR0         -- you just put down a seed
  | SeedR1         -- you watered the seed once
  | BuggedR        -- The seedling is bugged
  | Good_nw_nfR  -- The seedling is normal but not watered nor ferted
  | Good_w_nfR   -- The seedling is normal, watered but not ferted
  | Good_nw_fR   -- The seedling is normal, not watered but ferted
  | GoodR
  | GrownR
  | SeedA         -- you just put down a seed
  | BuggedA        -- The seedling is bugged
  | Good_nw_nfA  -- The seedling is normal but not watered nor ferted
  | Good_w_nfA   -- The seedling is normal, watered but not ferted
  | Good_nw_fA   -- The seedling is normal, not watered but ferted
  | GoodA
  | GrownA        -- It's a fully grown crop, you can now reap it to get score.
  | Bad           -- The plant is bad because you somehow mistreated your plant.
  deriving (Eq, Show)

data Pos = Pos 
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

(!) :: Board -> Pos -> Maybe Plant 
board ! pos = M.lookup pos board

dim :: Int
dim = 3

positions :: [Pos]
positions = [ Pos r c | r <- [1..dim], c <- [1..dim] ] 

emptyPositions :: Board -> [Pos]
emptyPositions board  = [ p | p <- positions, M.notMember p board]

takenPositions :: Board -> [Pos]
takenPositions board  = [ p | p <- positions, M.member p board]

init :: Board
init = M.empty

-------------------------------------------------------------------------------
-- | Playing a Move
-------------------------------------------------------------------------------

data Result a 
  = Retry
  | Cont a
  deriving (Eq, Functor, Show)


updatePlantWater :: Plant -> Maybe Plant
updatePlantWater x = 
  case x of  
    SeedR0      -> Just SeedR1
    SeedR1      -> Just Good_nw_nfR
    Good_nw_nfR -> Just Good_w_nfR
    Good_nw_fR  -> Just GoodR
    GoodR       -> Just GrownR
    SeedA       -> Just Good_nw_nfA
    Good_nw_nfA -> Just Good_w_nfA
    Good_nw_fA  -> Just GoodA
    GoodA       -> Just GrownA
    _           -> Just Bad

updatePlantFert :: Plant -> Maybe Plant
updatePlantFert x = 
  case x of  
    Good_nw_nfR -> Just Good_nw_fR
    Good_w_nfR  -> Just GoodR
    Good_nw_nfA -> Just Good_nw_fA
    Good_w_nfA  -> Just GoodA
    _           -> Just Bad

updatePlantDebug :: Plant -> Maybe Plant
updatePlantDebug x = 
  case x of  
    BuggedA -> Just Good_nw_nfA
    BuggedR -> Just Good_nw_nfR
    _    -> Just Bad

updatePlantBug :: Plant -> Maybe Plant
updatePlantBug x = 
  case x of 
    Good_nw_nfR -> Just BuggedR
    Good_w_nfR  -> Just BuggedR
    Good_nw_fR  -> Just BuggedR
    GoodR       -> Just BuggedR
    Good_nw_nfA -> Just BuggedA
    Good_w_nfA  -> Just BuggedA
    Good_nw_fA  -> Just BuggedA
    GoodA       -> Just BuggedA
    SeedR0      -> Just SeedR0  
    SeedR1      -> Just SeedR1
    SeedA       -> Just SeedA
    GrownA      -> Just GrownA
    GrownR      -> Just GrownR
    _           -> Just Bad

updatePlantDry :: Plant -> Maybe Plant
updatePlantDry x = 
  case x of 
    Good_w_nfR  -> Just Good_nw_nfR
    GoodR       -> Just Good_nw_fR
    Good_w_nfA  -> Just Good_nw_nfA
    GoodA       -> Just Good_nw_fA
    SeedR0      -> Just SeedR0  
    SeedR1      -> Just SeedR1
    SeedA       -> Just SeedA
    GrownA      -> Just GrownA
    GrownR      -> Just GrownR
    _           -> Just Bad

putSeedA :: Board -> Energy -> Pos -> Result Board
putSeedA board e pos = 
  if (getEnergy e) >= 20 
    then 
      case M.lookup pos board of 
          Nothing     -> result (M.insert pos SeedA board) 
          Just _      -> result board
    else
        result board  

putSeedR :: Board -> Energy -> Pos -> Result Board
putSeedR board e pos = 
  if (getEnergy e) >= 20 
    then 
      case M.lookup pos board of 
          Nothing     -> result (M.insert pos SeedR0 board) 
          Just _      -> result board
    else
        result board 

putWater :: Board -> Energy -> Pos -> Result Board
putWater board e pos =
  if (getEnergy e) >= 10 
    then case M.lookup pos board of 
        Just _  -> result (M.update updatePlantWater pos board)
        Nothing -> result board
    else 
        result board  

putFert :: Board -> Energy -> Pos -> Result Board
putFert board e pos =
  if (getEnergy e) >= 10 
    then 
      case M.lookup pos board of 
          Just _  -> result (M.update updatePlantFert pos board)
          Nothing -> result board
    else
        result board  

putDebug :: Board -> Energy -> Pos -> Result Board
putDebug board e pos =
  if (getEnergy e) >= 10 
    then 
      case M.lookup pos board of 
          Just _  -> result (M.update updatePlantDebug pos board)
          Nothing -> result board
    else
        result board 

putReap :: Board -> Energy -> Pos -> Result Board
putReap board e pos =
  if (getEnergy e) >= 20 
    then 
      case M.lookup pos board of 
          Just GrownR ->  result (M.delete pos board)
          Just GrownA ->  result (M.delete pos board)
          _          ->  result board
    else
        result board  

putClean :: Board -> Energy -> Pos -> Result Board
putClean board e pos =
  if (getEnergy e) >= 10 
    then 
      case M.lookup pos board of 
          Just Bad ->  result (M.delete pos board)
          _        ->  result board
    else
        result board 

-- putSleep :: Board -> Energy -> Pos -> Result Board
-- putSleep board e pos = case M.lookup pos board of
--   Just _  -> result (M.update updatePlantBug pos board)
--   Nothing -> result board

putSleep :: Board -> Int -> Pos -> Result Board
putSleep board acc pos = 
  if acc == 1 then
    case M.lookup pos board of
      Just _  -> result (M.update updatePlantDry pos board)
      Nothing -> result board
  else 
    case M.lookup pos board of
      Just _  -> result (M.update updatePlantBug pos board)
      Nothing -> result board

putEat :: Board -> Score -> Pos -> Result Board
putEat board _ _ =  result board

putConfirm :: Board -> Pos -> Result Board
putConfirm board _ =  result board

putRestart :: Board -> Pos -> Result Board
putRestart _ _ =  result M.empty

-- put :: Board -> Energy -> Pos -> Result Board
-- put board e pos = case getEnergy e of 
--   0 -> result board
--   _ -> case M.lookup pos board of 
--           Just Grown  -> result (M.delete pos board)
--           Nothing     -> result (M.insert pos Seed board) 
--           Just _      -> result (M.update (updatePlant e) pos board)


result :: Board -> Result Board
result b 
  | otherwise = Cont b

-- wins :: Board -> XO -> Bool
-- wins b xo = or [ winsPoss b xo ps | ps <- winPositions ]

-- winsPoss :: Board -> XO -> [Pos] -> Bool
-- winsPoss b xo ps = and [ b!p == Just xo | p <- ps ]

-- isFull :: Board -> Bool
-- isFull b = M.size b == dim * dim

-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------

up :: Pos -> Pos 
up p = p 
  { pRow = max 1 (pRow p - 1) 
  } 

down :: Pos -> Pos
down p = p 
  { pRow = min dim (pRow p + 1) 
  } 

left :: Pos -> Pos 
left p = p 
  { pCol   = max 1 (pCol p - 1) 
  } 

right :: Pos -> Pos 
right p = p 
  { pCol = min dim (pCol p + 1) 
  } 
