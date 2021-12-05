open CS17SetupGame;
open Game; 

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame
 
  /* nextMove takes in the state of the current game, and produces the best 
     possible move that will lead to a win for this player, implementing
        --limited depth minimax algorithm
        --estimateValue procedure
        */
  /* input: s, the current state of the game
     output: a move that is calculated to be the best move at the state s for 
             the AI player to win
     */
  let nextMove: (PlayerGame.state => PlayerGame.move) = s => {
    failwith("not yet implemented")
  }
  
  /* put your team name here! */
  let playerName = "";
  
};

module TestGame = Connect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame); 
module MyAIPlayer:Player = TestAIPlayer;
open TestAIPlayer; 

/* insert test cases for any procedures that don't take in 
 * or return a state here */

