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

    /* ====================== Game Status Helpers ============================*/
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
        | [[_], ..._] => [List.flatten(mat)]
        | [[_, ..._], ..._] => 
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
      /*input:  
          gBoard: a list of list of strings that represents the game board
        output:
          a new list of list of strings that represents all the diagonals from
          right to left of gBoard
        */

      let rec diagonalLeft: list(list(string)) => list(list(string)) = 
        gBoard => {
          /* input:  
              input: a list of string that represents the first column in a 
                     game board
              ro: a list of list of string of length 1, that represents the 
                  recursive output of diagonalHelper, always 1 element longer 
                  than ro
            output:
              a list of list of strings that pairs the first of input with first
              of ro, up until the last element of ro, which is its own string 
              list
            */
          let rec diagonalHelper: (list('a), list(list('a))) => list(list('a)) =
            (input, ro) =>
              switch(input, ro) {
              | ([], tl) => tl
              | ([hd, ...tl], tl2) => 
                  [[hd, ...List.hd(tl2)], ...diagonalHelper(tl, List.tl(tl2))]
              };
          /* recursion diagram
            oi: [" ", "O"], [[" ", "X", "X"]]
              ri: ["O"], [["O", "X"]]
              ro: [["O, "X"], ["X"]]
            is: cons [" ", " "] of input onto ro
            oo: [[" ", " "], ["O, "X"], ["X"]]

            oi: [" "], [[" ", "X"]]
              ri: [], [["X"]]
              ro: [["X"]]
            is: cons [" ", " "] of input onto ro
            oo: [[" ", " "], ["X"]]
            */
          switch(gBoard) {
          | [[_, ..._]] => transpose(gBoard)
          | [hd, ...tl] =>
             [[List.hd(hd)], ...diagonalHelper(List.tl(hd), diagonalLeft(tl))]
          | _ => failwith("diagonalLeft error")
         };
    };
     /* input:  
          gBoard: a list of list of strings that represents the game board
        output:
          a new list of list of strings that represents all the diagonals from
          left to right of gBoard
        */
    let diagonalRight: list(list(string)) => list(list(string)) =
      gBoard => 
        diagonalLeft(List.map(List.rev, gBoard))
    /* input: 
          _ => the current state of the game
       output:
          the status of the game
            the status of the game
            Win if either P1 or P2 has 4 of their pieces in a row
            Draw, if neither player has won and there is no legal moves available
            else, Ongoing whichever player's turn it is
       */
    let rec fourInCol: state => status = cState =>
        switch(cState.gameBoard) {
        | [[_, _, _], ...tl] => fourInCol({
                gameBoard: tl, 
                stateStatus: cState.stateStatus
                })
        | [[a, b, c, d, ..._], ..._] 
            when ([a, b, c, d] == ["X", "X", "X", "X"]) => Win(P1)
        | [[a, b, c, d, ..._], ..._] 
            when ([a, b, c, d] == ["O", "O", "O", "O"]) => Win(P2)
        | [[_, b, c, d, ...col1tl], ...tl] => fourInCol({
                gameBoard: [[b, c, d, ...col1tl], ...tl], 
                stateStatus: cState.stateStatus
                })
        | [_, ... _] when (legalMoves(cState) == []) => Draw
        | _ => cState.stateStatus
        };
        /* recursion diagram
            oi: {
              gameBoard: [[" ", "X", "X", "X"], [" ", "O", "O", "X"]],
              stateStatus: Ongoing(P1)
              }
              ri: [["O", "O", "X"]]
              ro: Ongoing(P1)
            is: return ro, since neither player has won, and is not a draw
            oo: Ongoing(P1)

            oi: {
              gameBoard: [["X", "X", "X", "X"], ["O", "O", "O", "X"]],
              stateStatus: Ongoing(P2)}
              ri: n/a
              ro: n/a
            is: np ri since there is 4 in a row, someone has won
            oo: Win(P1)
          */

    /* =======================================================================*/

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
    let gameStatus: state => status = currentState => 
      switch(fourInCol(currentState)) {
      | Win(P1) => Win(P1)
      | Win(P2) => Win(P2)
      | Draw => Draw
      | _ => switch(fourInCol({
              gameBoard: transpose(currentState.gameBoard),
              stateStatus: currentState.stateStatus
              })) {
              | Win(P1) => Win(P1)
              | Win(P2) => Win(P2)
              | Draw => Draw
              | _ => switch(fourInCol({
                      gameBoard: diagonalLeft(currentState.gameBoard),
                      stateStatus: currentState.stateStatus
                      })) {
                      | Win(P1) => Win(P1)
                      | Win(P2) => Win(P2)
                      | Draw => Draw
                      | _ => switch(fourInCol({
                              gameBoard: diagonalRight(currentState.gameBoard),
                              stateStatus: currentState.stateStatus
                              })) {
                              | Win(P1) => Win(P1)
                              | Win(P2) => Win(P2)
                              | Draw => Draw
                              | Ongoing(hd) => 
                                  switch(hd) {
                                  | P1 => Ongoing(P2)
                                  | P2 => Ongoing(P1)
                                  };
                              }; 
                            };
                          };
                        };

    /* given a state and a legal move, yields the next state */
    /* input: 
         cState: current state of the game
         move: a num representing a legal move
       output:
         a new game state with the next game board after move is made, and the 
         new state of the game, whether it's a draw, win, or ongoing 
       */
  let nextState: (state, move) => state =
    (cState, move) => {
      /* input: 
          col: a list of string that represents a column in gameboard to add a  
               piece
          p: whichPlayer, represents if it's P1 or P2's turn
       output:
          a new list of string that is a column of game board with the correct
          piece added
       */
    let rec addPiece: (list(string), whichPlayer) => list(string) = 
      (col, p) =>
        switch(col) {
        | [" "] when (p == P1) => ["X"]
        | [" "] when (p == P2) => ["O"]
        | [" ", _, ...tl] when (p == P1) => ["X", "X", ...tl]
        | [" ", _, ...tl] when (p == P2) => ["O", "X", ...tl]
        | [" ", "O", ...tl] when (p == P1) => ["X", "O", ...tl]
        | [" ", "O", ...tl] when (p == P2) => ["O", "O", ...tl]
        | [" ", " ", ...tl] => [" ", ...addPiece([" ", ...tl], p)]
        | _ => failwith("addPiece error")
        };
        /* recursion diagram
            oi: [" ", "X", "X", "X"], P1
              ri: n/a
              ro: n/a
            is: replace " " with "X", since base case
            oo: ["X", "X", "X", "X"]

            oi: [" ", " ", "X", "X"]
              ri: [" ", "X", "X"]
              ro: ["X", "X", "X"]
            is: cons " " onto ro
            oo: ["X", "X", "X", "X"]
          */
  
    /* input: 
          s: the current state of the game
          n: the column of game board to add a piece
       output:
          a new game board with the correct piece added in correct column
       */
    let rec nextGBoard: (state, move) => list(list(string)) = 
      (s, n) =>
        switch(s.gameBoard, n) {
        | ([hd, ...tl], Move(1)) when (s.stateStatus == Ongoing(P1)) => 
            [addPiece(hd, P1), ...tl]
        |([hd, ...tl], Move(1)) when (s.stateStatus == Ongoing(P2)) => 
            [addPiece(hd, P2), ...tl]
        | ([hd, ...tl], Move(n)) when (s.stateStatus == Ongoing(P1)) => 
            [hd, ...nextGBoard(
                {gameBoard: tl, stateStatus: s.stateStatus}, 
                Move(n-1)
                )
              ]
        | _ => failwith("nextGBoard error")
        };
        /* recursion diagram
            oi: {
              gameBoard: [[" ", "X", "X", "X"], [" ", "O", "O", "O"]],
              stateStatus: Ongoing(P1)
              }, Move(1)
              ri: n/a
              ro: n/a
            is: base case, addPiece to first col
            oo: [["X", "X", "X", "X"], [" ", "O", "O", "O"]]

            oi: {
              gameBoard: [["X", "X", "X", "X"], [" ", "O", "O", "X"]],
              stateStatus: Ongoing(P2)}, Move(2)
              ri: {
              gameBoard: [[" ", "O", "O", "X"]],
              stateStatus: Ongoing(P2)}, Move(1)
              ro: [["O", "O", "O", "X"]]
            is: cons hd of game of game board onto ro, return it
            oo: [["X", "X", "X", "X"], ["O", "O", "O", "X"]]
          */
    {
      gameBoard: nextGBoard(cState, move), 
      stateStatus: gameStatus(
        {
          gameBoard: nextGBoard(cState, move),
          stateStatus: cState.stateStatus
        }
      )
    }
    };


    /* for transforming human player input into
    internal representation of move */
    /* input:
         str: a string that is the user input for a move
         s: the current state of the game
       output:
         A move type of the str move if the move is a valid move, otherwise 
         prints an error message
       */
    let moveOfString: (string, state) => move = (str, s) => {
      let validMove = fun
        | str => 
          try(string_of_int(int_of_string(str))) {
          | _ => "Please input a number"
          }
      switch(int_of_string(validMove(str))) {
      | n when (List.mem(Move(n), legalMoves(s))) => Move(n)
      | _ => failwith("IMPLEMENT")
      }
    };


    /* estimates the value of a given state (static evaluation) */
    let estimateValue: state => float = cState => failwith("Not implemented");



    /* printing functions; converts a whichPlayer type, a state type, or a move
       type into a string */
        /* input: whichplayer: a player
           output: a string that represents the player
           */
        let stringOfPlayer: whichPlayer => string = p =>
          switch(p) {
          | P1 => "Player 1"
          | P2 => "Player 2"
          };

        /* input: 
              cState: the current state of the game
           output: 
              the representation of the current game board and a string 
              representing which player's turn it is, or if the game has ended
           */
        let rec stringOfState: state => string = cState => {
          /* input: 
              alos: a list of string that represents a row in the game board
           output: 
              the string representation of a single row in the game board 
           */
          let rec strLstToString: list(string) => string =
            alos =>
              switch (alos) {
              | [] => " | "
              | [hd, ...tl] => " | " ++ hd ++ strLstToString(tl)
              };
          let transposedGBoard = transpose(cState.gameBoard)
          switch(transposedGBoard) {
          | [] => ""
          // | [] => switch(cState.stateStatus) {
          //   | Win(p) => "\n" ++ stringOfPlayer(p) ++ " has won the game!"
          //   | Draw => 
          //       "\nThere are no more moves to make.  The game is a draw."
          //   | Ongoing(p) => 
          //      "\n" ++ stringOfPlayer(p) ++ " please make a move..."
          //   };
          | [hd, ...tl] => 
              strLstToString(hd) ++ 
              "\n" ++ 
              stringOfState(
                {gameBoard: tl, stateStatus: cState.stateStatus}
                )
            };
          };
        /* recursion diagram
            oi: {
              gameBoard: [[" ", "X", "X", "X"], [" ", "O", "O", "O"]],
              stateStatus: Ongoing(P1)
              }
              ri: {
              gameBoard: [[" ", "O", "O", "O"]],
              stateStatus: Ongoing(P1)
              }
              ro: |  | O | O | O |

                  "Player 1 please make a move..."
            is: print out the string representation of hd of oi with a newline
            oo: |   | X | X | X |
                |   | O | O | O |

                "Player 1 please make a move..."

            oi: {
              gameBoard: [[" ", "X", "X", "X"], [" ", "O", "O", "X"]],
              stateStatus: Ongoing(P2)}
              ri: {
              gameBoard: [[" ", "O", "O", "X"]],
              stateStatus: Ongoing(P2)}
              ro: |  | O | O | X |

                  "Player 2 please make a move..."
            is: print out the string representation of hd of oi with a newline
            oo: |   | X | X | X |
                |   | O | O | X |

                "Player 2 please make a move..."
          */

        /* input: m, a move
           output: a string representation of m, a move
           */
        let stringOfMove: move => string = m =>
          switch(m) {
          | Move(n) => string_of_int(n)
          }


};

module MyGame : Game = Connect4;
open Connect4;

/* test cases */
