# Farmsim
![image](https://github.com/TheSoulOfCorn/Project_21FALL_CSE230/blob/master/screenshot.png)

## Background
This is a farming simulator game based on the brick library. The relieving farming game has been popular for years, focusing on the virtual experience of farmer life with planting, breeding, and more. Our application is to simulate the world-famous game based on brick.
## Instructions

- The farmer has 3 days to grow crops. 
- There are 2 kinds of crops to grow: rice🌾 or apple🍎.
- Rice is grown faster than apple. Rice needs one watering to become seedling, one watering and one fertilizing to become normal seedling, and one watering to reap. Apple needs one watering to become seedling phase 1, one watering to become seedling phase 2, one watering and one fertilizing to become normal seedling, and one watering to reap.
- Each day 100 energy is allocated.
- The farmer can do the following operations: 
- Plant ( -20 energy )
- Water  ( -10 energy )
- Apply Fertilizer ( -10 energy )
- Eat Ripe Plant ( -1 score, +50 energy) 
- Clean Dead Plant ( -10 energy ) [The plant will go bad if water/fertilize too much, and should be clean]
- Sleep ( +100 energy,  - 1 date)  [You  must go to sleep if the energy turns to 0]
- Reap grown plants ( +1 score,  -20 energy )
- The game will end if date turns to 0, you can go to next round or exit the game, the maximum score will be recorded.
- The plant can be randomly affected by drought or infestation and go bad.
- More instructions is on the game panel.

## Team Members
yukiyukiyeah, Muyao-Liu, Onojimi, TheSoulOfCorn
## For Developers
* Compile with `stack build`
* Play with `stack run`
