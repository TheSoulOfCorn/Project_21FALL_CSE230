# Project_21FALL_CSE230
- **Farming simulator based on brick**
- team members: yukiyukiyeah,Muyao-Liu,Onojimi,TheSoulOfCorn
- This is a farming simulator game based on the brick library. The relieving farming game has been popular for years, focusing on the virtual experience of farmer life with planting, breeding, and more. Our application is to simulate the world-famous game based on brick.
- In this project, our main goal is to create a virtual plantation where the user can plant vegetables and fruits. The user can choose different game mode they prefer. The classic mode contains a settled loops (or days in our game) and the user has limited power, and he aims to get higher score in the limited time. The gamer has lands where he can plant, and he can choose whatever he like to plant.
- To balance the game, plants with shorter growing period yield less score than those longer. The plants suffer from pests, drought and so, which have the gamer to regularly water and take pest control. Of course, all these take the power of a day, so the user has to balance whether to plant a lot, or just focus on few. Since plants die from the drought and pest will even lower the score. The user has to take all the risk and have an elaborated plan of how to use up the power every day, though luck is part of the game.
- To embellish, we then introduce more uncertainties like the plant stealer, someone steals your plant and you get nothing. Also fertilizers, things help your plant grow faster and gain more score.
- Hopefully our strategy game would have a nice user interface to attract everyone.

- Updates
- The key components are farmer, field, crop.
- The fields have 2 status, it's either empty (no plant growing) or occupied by a crop.
- The crop has 4 status (seed, normal, abnormal, grown). The farmer can plant a seed on an empty field. Then, the seed can grow to become a normal seedling or an abnormal seedling. The farmer has to take actions to turn the abnormal seedling into a normal one. After that, the normal seedling can then grow to become a fully grown plant. The farmer can then reap the fully grown plant to gain score. 
- The farmer, field, crop are implemented by Player and Board datatype.
- The actions farmer are implemented by using first up, down, left, right keys to locate which square of field that he wants to operate on. And enter key to plant the seed, fix the abnormal plant or reap a fully grown plant. 
- The interface will consist of mainly 2 parts: the grid fields showing the current plantation and a status bar to show the farmer's score and status so far.
- We probably will not be able to add more uncertainties like the plant stealer and fertilizer into the project due to the time limitation.
