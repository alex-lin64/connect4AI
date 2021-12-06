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
        makeGameBoard: takes in a pair of integers, which is the heigh and the
        width of the gameboard and returns a list of list, which is a 
        representation of the gameboard with height as the number of elements in
        each inner list and width as the total number of inner lists.
        input: 
            h: int, height of the board
            w: int, width of the board 
        output:
            a list of list with w inner lists, each of which has h elements. 
            This is the representation of an initial gameboard with h rows and 
            w columns.
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

    /* legalMoves: produces the list of legal moves at a state 
       input: 
         currentState: current state of the gameboard
       output:
         a list of legal moves, where legal move is a column that is not filled 
         in the gameboard
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
    Data definition: matrix
    A matrix is a list of lists. The total number of inner lists is the height,
    and the number of elements in each inner list is the width of the matrix.
    
    transpose: convert a matrix into a new matrix, in which the rows and columns
    are switched
    input:
      a list of list of 'a, which represents a matrix 
    output:
      a list of list of 'a, which is a matrix reflected across its main diagonal
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
      
      /*
      diagonalLeft: 
      input:  
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
    let fourInCol: state => status = cState => {
      let rec fourInColHelper: (state, state) => status = (o, r) =>
        switch(r.gameBoard) {
        | [[a, b, c, d, ..._], ..._] 
            when ([a, b, c, d] == ["X", "X", "X", "X"]) => Win(P1)
        | [[a, b, c, d, ..._], ..._] 
            when ([a, b, c, d] == ["O", "O", "O", "O"]) => Win(P2)
        | [[_, b, c, d, ...col1tl], ...tl] => fourInColHelper(o,
                  {
                gameBoard: [[b, c, d, ...col1tl], ...tl], 
                stateStatus: cState.stateStatus
                })
        | [_, ...tl] => fourInColHelper(o, 
                {
                gameBoard: tl, 
                stateStatus: cState.stateStatus
                })
        | [] when (legalMoves(o) == []) => Draw
        | [] => cState.stateStatus
        };
      fourInColHelper(cState, cState);
    }
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
      | _ => switch(fourInCol({
              gameBoard: transpose(currentState.gameBoard),
              stateStatus: currentState.stateStatus
              })) {
              | Win(P1) => Win(P1)
              | Win(P2) => Win(P2)
              | _ => switch(fourInCol({
                      gameBoard: diagonalLeft(currentState.gameBoard),
                      stateStatus: currentState.stateStatus
                      })) {
                      | Win(P1) => Win(P1)
                      | Win(P2) => Win(P2)
                      | _ => switch(fourInCol({
                              gameBoard: diagonalRight(currentState.gameBoard),
                              stateStatus: currentState.stateStatus
                              })) {
                              | Win(P1) => Win(P1)
                              | Win(P2) => Win(P2)
                              | Ongoing(hd) => 
                                  switch(hd) {
                                  | P1 => Ongoing(P1)
                                  | P2 => Ongoing(P2)
                                    };
                              | _ => Draw
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
        | [" ", "X", ...tl] when (p == P1) => ["X", "X", ...tl]
        | [" ", "X", ...tl] when (p == P2) => ["O", "X", ...tl]
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
        | ([hd, ...tl], Move(n)) when 
            (s.stateStatus == Ongoing(P1)) || (s.stateStatus == Ongoing(P2)) => 
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
      let sStatus = gameStatus({
          gameBoard: nextGBoard(cState, move),
          stateStatus: cState.stateStatus
          });
      switch(sStatus) {
      | Ongoing(p) when (p == P1) =>
        {
          gameBoard: nextGBoard(cState, move), 
          stateStatus: Ongoing(P2)
        }
      | Ongoing(p) when (p == P2) => 
        {
          gameBoard: nextGBoard(cState, move), 
          stateStatus: Ongoing(P1)
        }
      | _ => {
                gameBoard: nextGBoard(cState, move), 
                stateStatus: sStatus
              }
      };
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
    let moveOfString: (string, state) => _ = (str, s) => {
      let validMove = fun
        | moveStr =>   
          switch(int_of_string(moveStr)) {
          | n when (List.mem(Move(n), legalMoves(s)))=> Move(n)
          | _ => failwith("validMove error")
          };
      try(validMove(str)) {
      | _ => failwith("\nNumerically challenged.  Try again\n")
        };
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
        let stringOfState: state => string = cState => {
          /* input: 
              alos: a list of string that represents a row in the game board
           output: 
              the string representation of a single row in the game board 
           */
          let rec strLstToString: list(string) => string =
            alos =>
              switch (alos) {
              | [] => " |"
              | [hd, ...tl] => " | " ++ hd ++ strLstToString(tl)
              };
          let transposedGBoard = transpose(cState.gameBoard)
          let rec stringOfStateHelper: state => string = cStateTwo =>
            switch(cStateTwo.gameBoard) {
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
                stringOfStateHelper(
                  {gameBoard: tl, stateStatus: cState.stateStatus}
                  )
              };
            stringOfStateHelper(
              {gameBoard: transposedGBoard, stateStatus: cState.stateStatus}
              );
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
              ro: "|  | O | O | O |"
                  "Player 1 please make a move..."
            is: print out the string representation of hd of oi with a newline
            oo: "|   | X | X | X |
                 |   | O | O | O |
                 Player 1 please make a move..."
            oi: {
              gameBoard: [[" ", "X", "X", "X"], [" ", "O", "O", "X"]],
              stateStatus: Ongoing(P2)}
              ri: {
              gameBoard: [[" ", "O", "O", "X"]],
              stateStatus: Ongoing(P2)}
              ro: "|   | O | O | X |
                   Player 2 please make a move..."
            is: print out the string representation of hd of oi with a newline
            oo: "|   | X | X | X |
                 |   | O | O | X |
                 Player 2 please make a move..."
          */



        /* input: m, a move
           output: a string representation of m, a move
           */
        let stringOfMove: move => string = m =>
          switch(m) {
          | Move(n) => string_of_int(n)
          };
  };

module MyGame : Game = Connect4;
open Connect4;

/* test cases */

  // /* makeCol */
  // checkExpect(makeCol(0), [], "makeCol empty list");
  // checkExpect(makeCol(3), [" ", " ", " "], "makeCol cons list");

  // /* makeGameBoard */
  // checkExpect(makeGameBoard(3, 0), [], "makeGameBoard test 1");
  // checkExpect(
  //   makeGameBoard(4, 4), 
  //   [[" ", " ", " ", " "], 
  //   [" ", " ", " ", " "], 
  //   [" ", " ", " ", " "], 
  //   [" ", " ", " ", " "]], 
  //   "makeGameBoard test 2");

  /* legalMoves */
  checkExpect(
  legalMoves({gameBoard: [[" ", " ", " ", " "], 
                          [" ", " ", " ", " "], 
                          [" ", " ", " ", " "], 
                          [" ", " ", " ", " "]], 
             stateStatus: Ongoing(P1)}), 
             [Move(1), Move(2), Move(3), Move(4)], "legalMoves test 1");
  checkExpect(
    legalMoves({gameBoard: [["X", "O", "X"], 
                            ["O", "X", "O"],
                            [" ", "X", "O"]], 
                stateStatus: Ongoing(P1)}), 
                [Move(3)], "legalMoves test 2");
  checkExpect(
    legalMoves({gameBoard: [["X", "O", "X"], 
                            ["O", "X", "O"],
                            ["O", "X", "O"]], 
                stateStatus: Ongoing(P2)}), 
                [], "legalMoves test 3");

  /* transpose */
  checkExpect(transpose([[1]]), [[1]], "1 row 1 column")
  checkExpect(transpose([[1], [2]]), [[1, 2]], "2 row 1 column")
  checkExpect(transpose([["a", "b"]]), 
                                    [["a"], ["b"]], 
                                    "1 row 2 column")
  checkExpect(transpose([[1, 2], [3, 4]]),
                                    [[1, 3], [2, 4]],
                                    "2 row 2 column")
  checkExpect(transpose([["o", "p", "q"], 
                                    ["r", "s", "t"], 
                                    ["u", "v", "w"]]),
                                    [["o", "r", "u"], 
                                    ["p", "s", "v"], 
                                    ["q", "t", "w"]],
                                    "3 row 3 column")

  /* diagonalLeft */
  checkExpect(diagonalLeft([["1", "2", "3"], 
                            ["1", "2", "3"],
                            [" ", "2", "3"]]), 
                          [["1"], ["2", "1"], ["3", "2", " "], ["3", "2"], ["3"]],
                          "diagonal left: case 1");
  checkExpect(diagonalLeft([["1", "2", "3"]]), 
                          [["1"], ["2"], ["3"]],
                          "diagonal left: 1 col");
  checkExpect(diagonalLeft([["1"], ["2"], ["3"]]), 
                          [["1"], ["2"], ["3"]],
                          "diagonal left: 1 row");

  /* diagonalRight */
  checkExpect(diagonalRight([["1", "2", "3"], 
                            ["1", "2", "3"],
                            [" ", "2", "3"]]), 
                          List.rev([[" "], ["1", "2"], ["1", "2", "3"], ["2", "3"], ["3"]]),
                          "diagonal right: case 1");
  checkExpect(diagonalRight([["1", "2", "3"]]), 
                          [["3"], ["2"], ["1"]],
                          "diagonalRight: 1 col");
  checkExpect(diagonalRight([["1"], ["2"], ["3"]]), 
                          [["1"], ["2"], ["3"]],
                          "diagonalRight: 1 row");

  /* fourInCol */
  checkExpect(
    fourInCol({gameBoard:  [[" ", " ", " ", " "], 
                            [" ", " ", " ", " "], 
                            [" ", " ", " ", " "], 
                            [" ", " ", " ", " "]], 
              stateStatus: Ongoing(P1)}), 
              Ongoing(P1), 
              "fourInCol: Ongoing");
  checkExpect(
    fourInCol({gameBoard:  [[" ", " ", "X", "O"], 
                            [" ", "O", "X", "X"], 
                            [" ", " ", "O", "X"], 
                            ["X", "O", "X", "O"]], 
              stateStatus: Ongoing(P2)}), 
              Ongoing(P2), 
              "fourInCol: Ongoing");
  checkExpect(
    fourInCol({gameBoard:  [["O", "O", "O", "O"], 
                            [" ", " ", " ", " "], 
                            [" ", " ", " ", " "], 
                            [" ", " ", " ", " "]], 
              stateStatus: Ongoing(P1)}), 
              Win(P2), 
              "fourInCol: O, vertical");
  checkExpect(
    fourInCol({gameBoard:  [[" ", " ", " ", " "], 
                            [" ", " ", " ", " "], 
                            [" ", " ", " ", " "],
                            ["X", "X", "X", "X"]], 
              stateStatus: Ongoing(P1)}), 
              Win(P1), 
              "fourInCol: X, vertical"); 

  /* gameStatus */
  checkExpect(
  gameStatus({gameBoard:  [[" ", " ", " ", " "], 
                          [" ", " ", " ", " "], 
                          [" ", " ", " ", " "], 
                          [" ", " ", " ", " "]], 
             stateStatus: Ongoing(P1)}), 
             Ongoing(P1), 
             "gameStatus: Ongoing");
checkExpect(
  gameStatus({gameBoard:  [[" ", " ", "X", "O"], 
                          [" ", "O", "X", "X"], 
                          [" ", " ", "O", "X"], 
                          ["X", "O", "X", "O"]], 
             stateStatus: Ongoing(P2)}), 
             Ongoing(P2), 
             "gameStatus: Ongoing");
checkExpect(
  gameStatus({gameBoard:  [["O", "O", "O", "O"], 
                          [" ", " ", " ", " "], 
                          [" ", " ", " ", " "], 
                          [" ", " ", " ", " "]], 
             stateStatus: Ongoing(P1)}), 
             Win(P2), 
             "gameStatus: O, vertical");
checkExpect(
  gameStatus({gameBoard:  [[" ", " ", " ", " "], 
                          [" ", " ", " ", " "], 
                          [" ", " ", " ", " "],
                          ["X", "X", "X", "X"]], 
             stateStatus: Ongoing(P1)}), 
             Win(P1), 
             "gameStatus: X, vertical");

checkExpect(
  gameStatus({gameBoard:  [[" ", " ", " ", "X"], 
                          [" ", " ", " ", "X"], 
                          [" ", " ", " ", "X"], 
                          [" ", " ", " ", "X"]], 
             stateStatus: Ongoing(P1)}), 
             Win(P1), 
             "gameStatus: X, hori");
checkExpect(
  gameStatus({gameBoard:  [[" ", " ", " ", "O"], 
                          [" ", " ", " ", "O"], 
                          [" ", " ", " ", "O"], 
                          [" ", " ", " ", "O"]], 
             stateStatus: Ongoing(P1)}), 
             Win(P2), 
             "gameStatus: O, hori");
checkExpect(
  gameStatus({gameBoard:  [[" ", " ", " ", "O"], 
                          [" ", " ", "O", "X"], 
                          [" ", "O", " ", "O"], 
                          ["O", " ", "X", "X"]], 
             stateStatus: Ongoing(P1)}), 
             Win(P2), 
             "gameStatus: O, diagLeft");
checkExpect(
  gameStatus({gameBoard:  [[" ", " ", " ", "X"], 
                          [" ", " ", "X", "X"], 
                          [" ", "X", " ", "O"], 
                          ["X", " ", "X", "X"]], 
             stateStatus: Ongoing(P1)}), 
             Win(P1), 
             "gameStatus: X, diagLeft");
checkExpect(
  gameStatus({gameBoard:  [["X", "O", "O", "O"], 
                          [" ", "X", "O", "X"], 
                          [" ", "X", "X", "O"], 
                          ["O", "X", "X", "X"]], 
             stateStatus: Ongoing(P1)}), 
             Win(P1), 
             "gameStatus: X, diagRight");
checkExpect(
  gameStatus({gameBoard:  [["O", "X", "X", "O"], 
                          [" ", "O", "O", "X"], 
                          [" ", "X", "O", "O"], 
                          ["O", "X", "X", "O"]], 
             stateStatus: Ongoing(P1)}), 
             Win(P2), 
             "gameStatus: O, diagRight");
checkExpect(
  gameStatus({gameBoard:  [["X", "O", "O", "O"], 
                           [" ", "X", "O", "X"], 
                           [" ", "X", "X", "O"], 
                           ["O", "X", "X", "X"]], 
             stateStatus: Ongoing(P1)}), 
             Win(P1), 
             "gameStatus: X, diagRight");
checkExpect(
  gameStatus({gameBoard:  [["X", "O", "O", "O"], 
                           ["X", "X", "O", "X"], 
                           ["X", "X", "O", "O"], 
                           ["O", "X", "X", "X"]], 
             stateStatus: Ongoing(P1)}), 
             Draw, 
             "gameStatus: draw");
checkExpect(
  gameStatus({gameBoard:  [["X", "O", "O", "O", "O", "X"], 
                           ["X", "X", "O", "X", "X", "O"], 
                           ["X", "X", "O", "O", "X", "X"], 
                           ["O", "X", "X", "O", "X", "O"],
                           ["O", "O", "X", "O", "O", "O"]], 
             stateStatus: Ongoing(P1)}), 
             Win(P2), 
             "gameStatus: Big board");
  
  /* nextState */
  checkExpect(
  nextState(
    {gameBoard:  [[" ", " ", " ", "O"], 
                  [" ", " ", "X", "X"], 
                  [" ", "X", "O", "O"], 
                  ["X", "O", "X", "X"]], 
             stateStatus: Ongoing(P1)},
    Move(1)),
    {gameBoard:  [[" ", " ", "X", "O"], 
                  [" ", " ", "X", "X"], 
                  [" ", "X", "O", "O"], 
                  ["X", "O", "X", "X"]], 
             stateStatus: Ongoing(P2)},
    "nextState: test 1"
  );
checkExpect(
  nextState(
    {gameBoard:  [[" ", " ", " ", "X"], 
                  [" ", " ", "X", "X"], 
                  [" ", "X", "O", "O"], 
                  [" ", "O", "X", "X"]], 
             stateStatus: Ongoing(P2)},
    Move(1)),
    {gameBoard:  [[" ", " ", "O", "X"], 
                  [" ", " ", "X", "X"], 
                  [" ", "X", "O", "O"], 
                  [" ", "O", "X", "X"]], 
             stateStatus: Ongoing(P1)},
    "nextState: test 2"
  );
checkExpect(
  nextState(
    {gameBoard:  [[" ", " ", " ", "X"], 
                  [" ", " ", "X", "X"], 
                  [" ", "X", "O", "O"], 
                  [" ", "O", "X", "X"]], 
             stateStatus: Ongoing(P1)},
    Move(4)),
    {gameBoard:  [[" ", " ", " ", "X"], 
                  [" ", " ", "X", "X"], 
                  [" ", "X", "O", "O"], 
                  ["X", "O", "X", "X"]], 
             stateStatus: Win(P1)},
    "nextState: test 3"
  );
checkExpect(
  nextState(
    {gameBoard:  [[" ", " ", " ", "X"], 
                  [" ", " ", "X", "X"], 
                  [" ", "O", "O", "O"], 
                  [" ", "O", "X", "X"]], 
             stateStatus: Ongoing(P2)},
    Move(3)),
    {gameBoard:  [[" ", " ", " ", "X"], 
                  [" ", " ", "X", "X"], 
                  ["O", "O", "O", "O"], 
                  [" ", "O", "X", "X"]], 
             stateStatus: Win(P2)},
    "nextState: test 4"
  );
checkExpect(
  nextState(
    {gameBoard:  [[" ", " ", " ", " "], 
                  [" ", " ", "X", "X"], 
                  [" ", "X", "O", "X"], 
                  [" ", "O", "X", "X"]], 
             stateStatus: Ongoing(P1)},
    Move(1)),
    {gameBoard:  [[" ", " ", " ", "X"], 
                  [" ", " ", "X", "X"], 
                  [" ", "X", "O", "X"], 
                  [" ", "O", "X", "X"]], 
             stateStatus: Win(P1)},
    "nextState: test 5"
  );
checkExpect(
  nextState(
    {gameBoard:  [[" ", " ", " ", " "], 
                  [" ", " ", "X", "O"], 
                  [" ", "X", "O", "O"], 
                  [" ", "O", "X", "O"]], 
             stateStatus: Ongoing(P2)},
    Move(1)),
    {gameBoard:  [[" ", " ", " ", "O"], 
                  [" ", " ", "X", "O"], 
                  [" ", "X", "O", "O"], 
                  [" ", "O", "X", "O"]], 
             stateStatus: Win(P2)},
    "nextState: test 6"
  );
checkExpect(
  nextState(
    {gameBoard:  [[" ", "X", "X", "X"], 
                  [" ", "O", "X", "X"], 
                  [" ", "X", "O", "X"], 
                  [" ", "O", "X", "O"]], 
             stateStatus: Ongoing(P2)},
    Move(1)),
    {gameBoard:  [["O", "X", "X", "X"], 
                  [" ", "O", "X", "X"], 
                  [" ", "X", "O", "X"], 
                  [" ", "O", "X", "O"]], 
             stateStatus: Win(P2)},
    "nextState: test 6"
  );
checkExpect(
  nextState(
    {gameBoard:  [[" ", "X", "X", "X"], 
                  [" ", "X", "X", "X"], 
                  [" ", "X", "X", "X"], 
                  [" ", "O", "X", "X"]], 
             stateStatus: Ongoing(P1)},
    Move(1)),
    {gameBoard:  [["X", "X", "X", "X"], 
                  [" ", "X", "X", "X"], 
                  [" ", "X", "X", "X"], 
                  [" ", "O", "X", "X"]], 
             stateStatus: Win(P1)},
    "nextState: test 7"
  );
checkExpect(
  nextState(
    {gameBoard:  [[" ", "O", "X", "X"], 
                  ["O", "O", "X", "X"], 
                  ["O", "X", "O", "O"], 
                  ["O", "O", "X", "X"]], 
             stateStatus: Ongoing(P1)},
    Move(1)),
    {gameBoard:  [["X", "O", "X", "X"], 
                  ["O", "O", "X", "X"], 
                  ["O", "X", "O", "O"], 
                  ["O", "O", "X", "X"]], 
             stateStatus: Draw},
    "nextState: test 8"
  );  

  /* moveOfString */
  checkExpect(
  moveOfString(
    "1", 
    {gameBoard:  [[" ", " ", " ", "O"], 
                          [" ", " ", "O", "X"], 
                          [" ", "O", " ", "O"], 
                          ["O", " ", "X", "X"]], 
             stateStatus: Ongoing(P1)}),
    Move(1),
    "moveOfString: test 1");
checkError(()=>moveOfString(
    "d", 
    {gameBoard:  [[" ", " ", " ", "O"], 
                          [" ", " ", "O", "X"], 
                          [" ", "O", " ", "O"], 
                          ["O", " ", "X", "X"]], 
             stateStatus: Ongoing(P1)}),
    "\nNumerically challenged.  Try again\n");

  /* stringOfPlayer */
  checkExpect(stringOfPlayer(P1), "Player 1", "stringOfPlayer: P1");
  checkExpect(stringOfPlayer(P2), "Player 2", "stringOfPlayer: P2");

  /* stringOfState */
checkExpect(stringOfState({
              gameBoard: [[" ", "X", "X", "X"], [" ", "O", "O", "O"]],
              stateStatus: Ongoing(P1)
              }),
" |   |   |
 | X | O |
 | X | O |
 | X | O |
",
               "stringOfState: test 1");
checkExpect(stringOfState(
    {gameBoard:  [[" ", "X", "X", "X"], 
                  [" ", "X", "X", "X"], 
                  [" ", "X", "X", "X"], 
                  [" ", "O", "X", "X"]], 
             stateStatus: Ongoing(P2)}),
" |   |   |   |   |
 | X | X | X | O |
 | X | X | X | X |
 | X | X | X | X |
",
               "stringOfState: test 2");

  /* stringOfMove */
  checkExpect(stringOfMove(Move(1)), "1", "stringOfMove: 1");
  checkExpect(stringOfMove(Move(9)), "9", "stringOfMove: 9");

  /* estimateValue */
