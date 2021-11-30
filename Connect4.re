open CS17SetupGame;   
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
        the position of each player's piece on the game board, where
          p1's move is represented by the string "X"
          p2's move is represented by the string "O"
        the status, whose turn it is, or whether any player has won or draw*/
    type state = {
      gameBoard: list(list(string)),
      stateStatus: status 
    };

    /* describes a move that a player can make, the column number */
    type move = Move(int);  

    /* the initial state of the game, includes:
          the height and width of connect4 board
          which player's turn begins the game 
       the board is represented by a list of list of strings, where each inner
          list is a column and each string inside the list is a row*/
let initialState: string => state =
      s => {
        let boardDims = parseBoardDims(s);
        let boardHeight = getBoardHeight(boardDims);
        let boardWidth = getBoardWidth(boardDims);

        /* 
        makeCol: takes in an integer and produces a column with specified number
        of elements. Each element is a string of " ", and the total number of 
        elements is the number of rows in the gameboard.

        input: rowNum, a int that is number of rows
        output: a list of string, that is a column with rowNum number of rows
           */
        let rec makeCol: int => list(string) = rowNum =>
          switch(rowNum) {
          | 0 => []
          | _ => [" ", ...makeCol(rowNum - 1)]
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

        /* 
        makeGameBoard: takes in a pair of integers, which is the width and the 
        height of the gameboard and returns a gameboard with input dimensions

        input: 
            w: int, width of the board
            h: int, height of the board
        output:
            a initial game board with w columns and h rows
           */

        let rec makeGameBoard: (int, int) => list(list(string)) = (h, w) => 
          switch(h, w) {
          | (_, 0) => [] 
          | (row, col) => [makeCol(row), ...makeGameBoard(row, (col - 1))]
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
    /* 
      input: 
        currentState: current state of the board
      output:
        a list of legal moves, where legal move is a column that is not filled 
      */
    let legalMoves: state => list(move) = currentState => {
      let rec legalMovesHelper: (state, int) => list(move) = (cState, col) =>
        switch(cState.gameBoard) {
        | [] => []
        | [[" ", ..._], ...tl] => 
            [Move(col), ...legalMovesHelper(
              {
                gameBoard: tl, 
                stateStatus: cState.stateStatus
                }, 
              (col+1))]
        | [[_, ..._], ...tl] => 
            legalMovesHelper(
              {
                gameBoard: tl, 
                stateStatus: cState.stateStatus
                }, 
              (col+1))
        | _ => failwith("Invalid gameboard")
        };
        /* recursion diagram
            oi: {gameBoard: [[" ", "X"], [" ", "O"]]}, 1
              ri: [[" ", "O"]], 2
              ro: [Move(2)]
            is: cons Move(1) onto ro since col 1 is legal move
            oo: [Move(1), Move(2)]

            oi: {gameBoard: [["X", "X"], [" ", "O"]]}, 1
              ri: [[" ", "O"]], 2
              ro: [Move(2)]
            is: return ro, since first col is full
            oo: [Move(2)]
          */
    legalMovesHelper(currentState, 1)
    };

    /*
    input:
      mat: a matrix('a) data type
    output:
      a matrix reflected across its main diagonal
      */
    let rec transpose: list(list('a)) => list(list('a)) =
      mat =>
        switch(mat) {
        | []
        | [[], ..._] => failwith("Domain error") 
        | [[hd], ..._] => [List.flatten(mat)]
        | [[hd, ...tl], ..._] => 
          [List.map(List.hd, mat), ...transpose(List.map(List.tl, mat))]
        };
    /* recursion diagrams
        oi: [[1, 2], [3, 4]]
          ri: [3, 4]
          ro: [1, 3]
        is: create list from first of every row and cons onto [1, 3]
        oo: [[1, 3], [2, 4]]
        oi: [[1, 2], [3, 4], [5, 6]]
          ri: 2, 4, 6
          ro: [2, 4, 6]
        is: create list from first of every row,, cons onto ro
        oo: [[1, 3, 5], [2, 4, 6]]
        */

    /* returns the status of the game at the given state */
    /* 
      input: 
        currentState: current state of the game, including gameBoard and status
      output:
        the status of the game
          Win if either P1 or P2 has 4 of their pieces in a row
          Draw, if neither player has won and there is no legal moves available
          else, Ongoing whichever player's turn it is
      */

    let gameStatus: state => status = currentState => {
      let rec fourInCol: state => status = _ =>
        switch(currentState.gameBoard) {
        | [[_, _, _], ...tl] => fourInCol({
                gameBoard: tl, 
                stateStatus: currentState.stateStatus
                })
        | [[a, b, c, d, ...col1tl], ...tl] 
            when ([a, b, c, d] == ["X", "X", "X", "X"]) => Win(P1)
        | [[a, b, c, d, ...col1tl], ...tl] 
            when ([a, b, c, d] == ["O", "O", "O", "O"]) => Win(P2)
        | [[_, b, c, d, ...col1tl], ...tl] => fourInCol({
                gameBoard: [[b, c, d, ...col1tl], ...tl], 
                stateStatus: currentState.stateStatus
                })
        | [_, ... _] when (legalMoves(currentState) == []) => Draw
        | _ => currentState.stateStatus
        };
        /* recursion diagram
            oi: {gameBoard: [[" ", "X", "X", "X"], [" ", "O", "O", "X"]]}
              ri: [["O", "O", "X"]]
              ro: Ongoing(P1)
            is: return ro, since neither player has won, and is not a draw
            oo: Ongoing(P1)

            oi: {gameBoard: [["X", "X", "X", "X"], ["O", "O", "O", "X"]]}
              ri: n/a
              ro: n/a
            is: np ri since there is 4 in a row, someone has won
            oo: Win(P1)
          */
      /* 
        input:
        output:
        */
      let rec diagonalRight: list(list(string)) => list(list(string)) = 
        gBoard => 
          switch(gBoard) {
          | [[_]] => transpose(gBoard)
          | [[hd1, ...tl1], []] => [[hd1], ... ]
          |
         }



    }

    

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
