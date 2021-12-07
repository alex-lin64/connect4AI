* A user determines the size of the game board and changes from Human vs. Human to Human vs. AI in the referee module. After that, running the referee script gives a visual representation of the game board in the terminal. At there, the user should input a number from 1 to the number of columns of the game board to make a move. The turns are alternated by the referee, and the game will stop once either player has won or there is a draw.  

* The game is started by running the referee module. The referee module initializes the game by calling the initialState from the Connect4 game module and prompts in the terminal for a player to make a move. If it is a human's turn, the user types a column number in the terminal, which is converted into an internal representation of a move by string of move. After, nextState takes in that move and the current state in order to produce a new game state with that move added. If it is an AI's turn, the AI module will run nextMove, which runs the minimax algorithm. The minimax algorithm takes in a list of legalMoves from the current state and estimates how good the each resulting state is by estimateValue. Depending on how deep the minimax algorithm is initialized to search, the minimax will take either the max or the min of the float representing how good gameState is (max if it is P1's turn, min if P2). The move corresponding to the highest value resulting from the minimax algorithm will be the move the AI makes. The referee will switch turns between Human vs. Human, Human vs. AI, and AI vs. AI. The game ends when gameStatus determines a draw or a win. 

* 
1) gameStatus determines whether a player has won, but checking for wins or draw should have been in nextState. 
2) We don't know if it is a bug, but when inputting the dimension in the referee module, the width ends up representing the number of columns and the height ends up representing the number of rows.  

* Alex Lin and Hao Wen

* None
