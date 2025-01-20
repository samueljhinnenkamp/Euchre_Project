Euchre is a trick-taking card game that is played by 4 players on 2
teams. For each round of Euchre, each player gets dealt 5 cards that 
range from 9 to ace. Once the cards are dealt, the players determine 
what the trump suit should be, depending on the cards in hand. 
A nine (weakest rank) of the trump suit is more powerful than the ace 
(strongest rank) of a non-trump suit. When a player is selecting trump, 
the goal is for their team to win the majority of 5 tricks.

When deciding to bid trump, there is no straightforward process for
doing so. Therefore, this project determines what factors contribute
to creating a good hand that is worthy to bid on. Since there are no
datasets online that will achieve this goal, 300,000 Euchre rounds were
simulated Monte Carlo style. Information about the hand and the call
that should have been made based on the results have been recorded.
Multiclass machine learning algorithms get used to determine if 
a suit in hand should be passed on, bid trump, or be bid and played alone.

Check out this [document](https://rpubs.com/samueljhinnenkamp/MakingEuchreBidsWithMachineLearning) that dives into the modeling techniques used to best determine the best decisions to make on different simulated Euchre hands. 

Check out this [interactive app](https://samueljhinnenkamp.shinyapps.io/Euchre_App/) that uses the best created models from the above document to make suggestions in real-time. 

