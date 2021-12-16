open CS17SetupGame;
open Game; 

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame
 
  /* =============================== Helpers =================================*/
    /* maxF finds the largest float in a list of floats */
    /* input: alof: a non empty list of floats
       output: the largest of the floats
       */
    let rec maxF: list(float) => float = alof => 
      switch(alof) {
      | [] => failwith("maxF: list is empty")
      | [hd] => hd
      | [hd, ...tl] => 
          if(hd >= List.hd(tl)) {
            maxF([hd, ...List.tl(tl)])
          } else {
            maxF(tl)
          };
      };
    /* recursion diagrams
      oi: [2.3, 66.5]
        ri: [66.5]
        ro: 66.5
      is: since 2.3 is less than 66.5, return on tl of oi, return hd of tl since 
          tl is one element long
      oo: 66.5

      oi: [66.5]
        ri: n/a
        ro: n/a
      is: return hd of oi, since it is one element long
      oo: 66.5
      */
    
    /* minF finds the largest float in a list of floats */
    /* input: alof: a list of floats
       output: the smallest of the floats
       */
    let rec minF: list(float) => float = alof => 
      switch(alof) {
      | [] => failwith("minF: list is empty")
      | [hd] => hd
      | [hd, ...tl] => 
          if(hd <= List.hd(tl)) {
            minF([hd, ...List.tl(tl)])
          } else {
            minF(tl)
          };
      };
    /* recursion diagrams
      oi: [2.3, 66.5]
        ri: n/a
        ro: n/a
      is: since 2.3 is less than 66.5, return hd of oi
      oo: 2.3

      oi: [66.5, -2.3]
        ri: [-2.3]
        ro: -2.3
      is: return ro
      oo: -2.3
      */


    /* bestMove finds the best move to take based on a given estimateValue of 
        each move */
    /* input: 
          alom: a list of possible moves, same length as alof
          alof: a list of floats, each corresponding to a move in alom that 
                represents how good each move is
                    first float in alof corresponds to first move in alom, so on
          f: a float that represents the largest/smallest value based on minimax
       output:
          a move associated with the best move f based on its corresponding 
          float
       */
    let rec bestMove: 
      (list(PlayerGame.move), list(float), float) => PlayerGame.move = 
        (alom, alof, f) =>
          switch(alom, alof) {
          | ([hdM, ..._], [hdF, ..._]) when (f == hdF) => hdM
          | ([_, ...tlM], [_, ...tlF]) => bestMove(tlM, tlF, f)
          | _ => failwith("The float doesn't correspond to a move")
          };
    /* recursion diagrams
      oi: ([Move(2), Move(3)], [2.3, 66.5], 2.3)
        ri: n/a
        ro: n/a
      is: base case, since 2.3 already equals first of the list of floats
          return hd of list of moves
      oo: Move(2)

      oi: ([Move(2), Move(3)], [2.3, 66.5], 66.5)
        ri: [Move(3), [66.5], 66.5]
        ro: Move(3)
      is: return ro
      oo: Move(3)
      */
    

    /* foldMap creates a 'c list by applying a procedure that takes in a 'a list 
        and a second element 'b to produce a 'c type */
    /* input: 
         f: a procedure with type signature ('a, 'b) => 'c, that takes in a list 
            of 'a and an element 'b --> that is, minimax
         aloa: a list of 'a
         b: an element of type 'b
       output:
         a list of 'c that is result of applying f to every element within aloa
       */
    let rec foldMap: (('a, 'b) => 'c, list('a), 'b) => list('c) = 
      (f, aloa, b) =>
         switch(aloa) {
         | [] => []
         | [hd, ...tl] => [f(hd, b), ...foldMap(f, tl, b)]
         };
    /* recursion diagrams
      oi: max, [Move(2), Move(3)], 1
        ri: n/a
        ro: n/a
      is: run estimateValue on the list of moves
      oo: [2.3, 4.1]

      oi: max, [Move(2), Move(3)], 2
        ri: min, List.map(legalMoves, aLstOfStates([Move(2), Move(3)], state))
        ro: [2.3, 4.1]
      is: return ro, result of running minF on the List.map recursive input
      oo: [2.3, 4.1]
      */

  /* =========================================================================*/

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
    
    /* aLstOfStates converts a list of moves and a state into a list of all 
          possible next states */
    /* input: 
         alom: a list of possible legal moves for a given state
         s: the current state at any point in minimax predictions
       output:
         a list of possible states of the game based on a list of legal moves
       */
    let rec aLstOfStates: 
      (list(PlayerGame.move), PlayerGame.state) => list(PlayerGame.state) =
        (alom, s) =>
          switch(alom) {
          | [] => []
          | [hd, ...tl] => [PlayerGame.nextState(s, hd), ...aLstOfStates(tl, s)]
          };
  
    /* the minimax algorithm, estimates the value of all possible next moves 
          based on estimateValue
          the algorithm determines the min value, the worst for the AI, when its
            the opponents turn, and the max value, the best for the AI, when its
            the AI's turn.  These values are propogated up to the current state
            (the root), where the max value is taken to decide the best move for
            the AI to make
            */
    /* input: cState, the current state of the game
       output: a move that is the best move determined by the minimax algorithm 
               for the AI to win the game
       */
    let rec minimax: (PlayerGame.state, int) => float = 
      (cState, d) => {
        switch(PlayerGame.gameStatus(cState)) {
        | Ongoing(P1) when (d == 0) =>
            maxF(List.map(
              PlayerGame.estimateValue, 
              aLstOfStates(PlayerGame.legalMoves(cState), cState)
              ))
        | Ongoing(P2) when (d == 0) =>
            minF(List.map(
              PlayerGame.estimateValue, 
              aLstOfStates(PlayerGame.legalMoves(cState), cState)
              ))
        | Ongoing(P1) => 
            maxF(foldMap(
                  minimax, 
                  aLstOfStates(PlayerGame.legalMoves(cState), cState), 
                  (d-1)
                  )
                )
        | Ongoing(P2) =>
              minF(foldMap(
                  minimax, 
                  aLstOfStates(PlayerGame.legalMoves(cState), cState), 
                  (d-1)
                  )
                )
        | Win(P1) => 10000.0
        | Win(P2) => -8000.0
        | Draw => 0.0
        };
      };

      let depth = 3;  // depth of minimax predictions
      let posMoves = PlayerGame.legalMoves(s);  // possible moves in the game
      let argMax = minimax(s, depth);  // best float of a state according to 
                                       // minimax
      // estimateValue of each posMoves
      let posValues = foldMap(minimax, aLstOfStates(posMoves, s), (depth-1))
      bestMove(posMoves, posValues, argMax); 

      };
  
  /* put your team name here! */
  let playerName = "Chad";
  };

module TestGame = Connect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame); 
module MyAIPlayer:Player = TestAIPlayer;
open TestAIPlayer; 

/* insert test cases for any procedures that don't take in 
 * or return a state here */

  /* maxF */
  checkExpect(maxF([1.2]), 1.2, "maxF: 1 element list");
  checkExpect(maxF([1.2, 5.0, 1.3]), 5.0, "maxF: 3 element list");
  checkError(()=>maxF([]), "maxF: list is empty");

  /* minF */
  checkExpect(minF([1.2]), 1.2, "minF: 1 element list");
  checkExpect(minF([1.2, 5.0, -1.3]), -1.3, "minF: 3 element list");
  checkError(()=>minF([]), "minF: list is empty");  

  /* bestMove */
  checkExpect(
    bestMove([Move(2), Move(3)], [2.3, 66.5], 2.3), Move(2), "bestMove: test 1"
    );
  checkExpect(
    bestMove([Move(2), Move(3)], [2.3, 66.5], 66.5), Move(3), "bestMove: test 2"
    );
  checkError(
    ()=>bestMove([Move(2), Move(3)], [2.3, 66.5], 1.5), 
    "The float doesn't correspond to a move"
    );
