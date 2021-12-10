module View (view, myMap) where

import Brick
import Brick.Widgets.Center 
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
view s = [gameEnd s, withAttr alertAttr $ (needSleep s), center $ padRight (Pad 2) inst_sc <+> (withAttr fieldAttr(field s))]
  where inst_sc = hLimit 50 $ vTile[withAttr instAttr $ showInst, withAttr scoreAttr $ (showStats s)]

instAttr = attrName "instruction"
scoreAttr = attrName "score"
fieldAttr = attrName "field"
alertAttr = attrName "alert"

myMap :: PlayState -> AttrMap
myMap _ = attrMap defAttr
  [
    (instAttr, fg yellow ), 
    (scoreAttr, fg brightBlue ),
    (fieldAttr, fg green),
    (alertAttr, fg brightRed)
  ]

field :: PlayState -> Widget String
field s = 
  withBorderStyle unicode $
    borderWithLabel (str ("FarmSim")) $
      vTile [ mkRow s row | row <- [1..dim] ]

gameEnd :: PlayState -> Widget String
gameEnd s = 
  if psEnd s == True
    then withBorderStyle unicodeBold
        $ borderWithLabel (str ("Game is Over!"))
        $ center
        $ vBox [str (getEnd s)]
    else emptyWidget

needSleep :: PlayState -> Widget String
needSleep s = 
  if psEnerLow s == True
    then(withBorderStyle unicodeBold
        $ borderWithLabel (str ("Alert!"))
        $ center
        $ vBox [str (getSleep s)]) 
    else emptyWidget

showInst :: Widget String
showInst = withBorderStyle unicodeBold
  $ borderWithLabel (str ("Instructions"))
  $ center
  $ hLimit 80
  $ vBox [str getInst,
          str getInst2,
          str getInst3,
          str getInst4,
          str getInst5,
          str getInst6]

showStats :: PlayState -> Widget String
showStats s = 
  withBorderStyle unicodeBold
  $ borderWithLabel (str ("Score"))
  $ center
  $ hLimit 80
  $ vBox [str (getStats s)]


getStats :: PlayState -> String
getStats s = printf "Current Energy = %d,\nScore = %d,\nDate = %d\n" (getEnergy p) (getCurrScore sc) (getDate d)
      where p = psEnergy s
            sc = psScore s
            d = psDate s

getInst :: String
getInst = printf "Press O to plant an Apple,\nPress P to plant a rice,\nPress W to water a plant,\nPress F to apply fertilizer,\nPress C to clean up dead plant,\nPress E to eat up plant\nPress R to reap plant\nBlank Space to Sleep"

getInst2 :: String
getInst2 = printf "\nWhen plants need water their leaves are shrinking \nlike this /\\/\\\n"

getInst3 :: String
getInst3 = printf "\nWhen plants need fertilizer their stems are twisted \n"

getInst4 :: String
getInst4 = printf "\nWhen plants are bugged their leaves are abnormal\nlike this X X \n"

getInst5 :: String
getInst5 = printf "\nWhen plants are dead their leaves are falling\n"

getInst6 :: String
getInst6 = printf "\nToo much water/fertilizer can kill the plant\n"

getEnd :: PlayState -> String
getEnd s = printf "Your final score is %d.\nMaxScore is %d.\nPress Enter to restart the game.\nOr press Esc to exit." (getCurrScore sc) (getMaxScore sc)
      where sc = psScore s

getSleep :: PlayState -> String
getSleep s = printf "You don't have enough energy now!\n\nPress blank space to sleep.\nPress E to to one of your grown plant.\nPress A to getback to game.\nNow you have %d scores %d energy" (getCurrScore sc) (getEnergy p)
      where sc = psScore s
            p = psEnergy s

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
mkPlant Nothing = blockB
mkPlant (Just SeedR0) = blockSeedR0
mkPlant (Just SeedR1) = blockSeedR1
mkPlant (Just SeedA) = blockSeedA
mkPlant (Just Good_nw_fR) = blockNeedW
mkPlant (Just Good_nw_nfR) = blockNeedWF
mkPlant (Just Good_w_nfR) = blockNeedF
mkPlant (Just GoodR) = blockNormal
mkPlant (Just Good_nw_fA) = blockNeedW
mkPlant (Just Good_nw_nfA) = blockNeedWF
mkPlant (Just Good_w_nfA) = blockNeedF
mkPlant (Just GoodA) = blockNormal
mkPlant (Just BuggedA) = blockAbnormal
mkPlant (Just BuggedR) = blockAbnormal
mkPlant (Just Bad) = blockBad
mkPlant (Just GrownA) = blockGrownA
mkPlant (Just GrownR) = blockGrownR

blockB, blockSeedR0, blockSeedR1, blockSeedA, blockNormal, blockAbnormal, blockGrownA, blockGrownR :: Widget n
blockB = vBox (replicate 5 (str "     "))

blockSeedR0  = vBox [ str "     "
                    , str "     "
                    , str "  _  "
                    , str " / \\ "
                    , str " \\_/ "] 
                   
blockSeedR1  = vBox [ str "     "
                    , str "     "
                    , str "  *  "
                    , str " / \\ "
                    , str " \\_/ "]
               
blockSeedA  = vBox [ str "     "
                   , str "     "
                   , str "  x  "
                   , str " / \\ "
                   , str " \\_/ "]  

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
        
blockGrownR =   vBox [ str "  O  "
                     , str "o | o"
                     , str " \\|/ "
                     , str "  |  "
                     , str "  |  "]

blockGrownA =    vBox [ str "     "
                      , str "     "
                      , str " _|_ "
                      , str "(___)"
                      , str "     "]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget