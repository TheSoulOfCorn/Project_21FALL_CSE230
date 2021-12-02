# Farming Simulator 

Super minimal tic-tac-toe to illustrate use of `brick`

## Instructions

* Compile with `stack build`
* Play with `stack run`
	- Use cursor keys to move around
	- The farmer is given 3 days and 100 energy each day. 
	- water/fertilizing/reap takes energy, when energy = 0, you can only press enter to sleep or press e to eat one of your own reaped plant to gain energy.
	- Eating gives 50 energy, sleeping gives max (100) energy.
	- press o to plant a rice seed, press p to plant a apple seed.
	- press w for water, press f for fertilizer, press r to reap grown plant and gain 1 score.
	- planting/reaping takes 20 energy, watering/fertilizing takes 10 energy, Eating/sleeping doesn't take energy.
	- To properly grow a rice, do the following steps: 
		1. water two times to turn a seed into a seedling
		2. water once and fertilize once to turn a seedling into a grown rice
	- To properly grow an Apple, do the following steps: 
		1. water one time to turn a seed into a seedling
		2. water once and fertilize once to turn a seedling into a grown apple
	- Notice, if you water/fertilize too much, the plant will go bad. You can only press c to clean the bad plant to continue. Cleaning takes 10 enegy.
## TODO

- [ ] Add starting page and ending page
- [ ] Make the plants be accidentally bugged when the farmer is sleeping



