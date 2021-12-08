module View (view) where

import Brick
import Brick.Widgets.Center (center, )
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode, unicodeBold)

import Text.Printf (printf)

import Model
import Model.Board
import Model.Score
import Model.Energy
import Model.Date
import Graphics.Vty hiding (dim)

-- -------------------------------------------------------------------------------
-- view :: PlayState -> [Widget String]
-- -------------------------------------------------------------------------------
-- view s = [showStats s, field s]

view :: PlayState -> [Widget String]
view s = [gameEnd s, center $ padRight (Pad 2) inst_sc <+> (field s)]
  where inst_sc = hLimit 50 $ vTile[showInst, (showStats s)]

field :: PlayState -> Widget String
field s = 
  withBorderStyle unicode $
    borderWithLabel (str (getStats s)) $
      vTile [ mkRow s row | row <- [1..dim] ]

gameEnd :: PlayState -> Widget String
gameEnd s = 
  if psEnd s == True
    then withBorderStyle unicodeBold
        $ borderWithLabel (str ("Instructions"))
        $ center
        $ hLimit 50
        $ vBox [str  (getStats s)]
    else emptyWidget

showInst :: Widget String
showInst = withBorderStyle unicodeBold
  $ borderWithLabel (str ("Instructions"))
  $ center
  $ hLimit 50
  $ vBox [str getInst]

showStats :: PlayState -> Widget String
showStats s = withBorderStyle unicodeBold
  $ borderWithLabel (str ("Score"))
  $ center
  $ hLimit 50
  $ vBox [str (getStats s)]


getStats :: PlayState -> String
getStats s = printf "Current Energy = %d,\nScore = %d,\nDate = %d\n" (getEnergy p) (getScore sc) (getDate d)
      where p = psEnergy s
            sc = psScore s
            d = psDate s

getInst :: String
getInst = printf "Press O to plant an Apple,\npress P to plant a rice"


mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..dim] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c 
  | isCurr s r c = withCursor raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c = center (mkPlant xoMb)
  where 
    xoMb      = psBoard s ! Pos r c
    -- xoMb 
    --   | r == c    = Just X 
    --   | r > c     = Just O 
    --   | otherwise = Nothing

mkPlant :: Maybe Plant -> Widget n
mkPlant Nothing  = blockB
mkPlant (Just SeedR0)   = blockSeedR0
mkPlant (Just SeedR1)   = blockSeedR1
mkPlant (Just SeedA)   = blockSeedA
mkPlant (Just Good_nw_fR) = blockNeedW
mkPlant (Just Good_nw_nfR) = blockNeedWF
mkPlant (Just Good_w_nfR) = blockNeedF
mkPlant (Just Good_nw_fA) = blockNeedW
mkPlant (Just Good_nw_nfA) = blockNeedWF
mkPlant (Just Good_w_nfA) = blockNeedF
mkPlant (Just BuggedA) = blockAbnormal
mkPlant (Just BuggedR) = blockAbnormal
mkPlant (Just Bad) = blockBad
mkPlant (Just GrownA) = blockGrown
mkPlant (Just GrownR) = blockGrown

blockB, blockSeedR0, blockSeedR1, blockSeedA, blockNormal, blockAbnormal, blockGrown :: Widget n
blockB = vBox (replicate 5 (str "     "))

blockSeedR0  = vBox [ str "     "
                   , str "     "
                   , str "  _  "
                   , str " / \\ "
                   , str " \\_/ "] 
                   
blockSeedR1  = vBox [ str "     "
                   , str "  *  "
                   , str "  *  "
                   , str " / \\ "
                   , str " \\_/ "]

                
blockSeedA  = vBox [ str "     "
                   , str "  *  "
                   , str " * * "
                   , str "  *  "
                   , str "     "] 

blockNormal = vBox [ str "     "
                   , str "\\   /"
                   , str " \\ / "
                   , str "  |  "
                   , str "  |  "]

blockNeedW  = vBox [ str "     "
                   , str "     "
                   , str "/\\ /\\"
                   , str "  |  "
                   , str "  |  "]

blockNeedF  = vBox [ str "     "
                   , str "\\   /"
                   , str " \\ / "
                   , str "  /  "
                   , str "  \\  "]

blockNeedWF = vBox [ str "     "
                   , str "     "
                   , str "/\\ /\\"
                   , str "  /  "
                   , str "  \\  "]

blockAbnormal = vBox [ str "     "
                     , str "     "
                     , str " X X "
                     , str "  |  "
                     , str "  |  "]

blockBad =      vBox [ str "     "
                     , str "     "
                     , str "     "
                     , str " /|\\ "
                     , str "  |  "]
        
blockGrown =    vBox [ str "  O  "
                     , str "o | o"
                     , str " \\|/ "
                     , str "  |  "
                     , str "  |  "]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget