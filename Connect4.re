open!  CS17SetupGame;   
open Game; 

module Connect4 = {

    /* player 1 is P1, player 2 is P2 */
    type whichPlayer =
      | P1
      | P2;

    /* either a player has won, it's a draw, or it's ongoing */
    type status =
      | Win(whichPlayer)
      | Draw
      | Ongoing(whichPlayer);


    /* the state of the game: 
        the position of each player's piece on the game board
        the status, whose turn it is, or whether any player has won or draw*/
    type state = {
      gameBoard: list(list(string)),
      stateStatus: status 
    };

    /* describes a move that a player can make, the column number */
    type move = Move(int);  

    /* the initial state of the game, includes:
          the height and width of connect4 board
          which player's turn begins the game */
    let initialState: string => state =
      s => {
        let boardDims = parseBoardDims(s);
        let boardHeight = getBoardHeight(boardDims);
        let boardWidth = getBoardWidth(boardDims);
        /* input: colNum, a int that is number of columns
           output: a list of string, that is a row with colNum number of columns
           */
        let rec makeRow: int => list(string) = colNum =>
          switch(colNum) {
          | 0 => []
          | _ => [" ", ...makeRow(colNum -1)]
          };
        /* recursion diagram
            oi: 1
              ri: 0
              ro: []
            is: cons [" "] onto ro
            oo: [" "]

            oi: 2
              ri: 1
              ro: [" "]
            is: cons " " onto ro
            oo: [" ", " "]
          */

        /* input: 
              w: int, width of the board
              h: int, height of the board
           output:
              a initial game board with w columns and h rows
           */
        let rec makeGameBoard: (int, int) => list(list(string)) = (w, h) => 
          switch(w, h) {
          | (_, 0) => [] 
          | (col, row) => [makeRow(col), ...makeGameBoard(col, (row - 1))]
          | _ => failwith("At least 1 row and 1 column")
          };
        /* recursion diagram
            oi: 1, 2
              ri: 1, 1
              ro: [[" "]]
            is: cons [" "] onto ro
            oo: [[" "], [" "]]

            oi: 2, 1
              ri: 2, 0
              ro: [[]]
            is: cons [" ", " "] onto ro
            oo: [[" ", " "]]
          */
        {
          gameBoard: makeGameBoard(boardWidth, boardHeight),
          stateStatus: Ongoing(P1)
        };
      };

    /* produces the list of legal moves at a state */
    let legalMoves: state => list(move);

    /* returns the status of the game at the given state */
    let gameStatus: state => status;

    /* given a state and a legal move, yields the next state */
    let nextState: (state, move) => state;

    /* for transforming human player input into
    internal representation of move */
    let moveOfString: (string, state) => move;

    /* estimates the value of a given state (static evaluation) */
    let estimateValue: state => float;


    /* printing functions */
        let stringOfPlayer: whichPlayer => string;
        let stringOfState: state => string;
        let stringOfMove: move => string;


};

module MyGame : Game = Connect4;
open Connect4;

/* test cases */
