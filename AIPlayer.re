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
    
    /* maxF finds the largest float in a list of floats */
    /* input: alof: a list of floats
       output: the largest of the floats
       */
    let maxF: list(float) => float = alof => {
      let rec maxFIter: (list(float), float) => float = (alofH, n) => 
        switch(alofH) {
        | [] => n
        | [hd] when (hd >= n) => hd
        | [hd] => n
        | [hd, ...tl] => 
            if(hd >= List.hd(tl)) {
              maxFIter(tl, hd)
            } else {
              maxFIter(tl, List.hd(tl))
            };
        };
      maxFIter(alof, 0.0)
    };
    
    /* minF finds the largest float in a list of floats */
    /* input: alof: a list of floats
       output: the smallest of the floats
       */
    let minF: list(float) => float = alof => {
      let rec minFIter: (list(float), float) => float = (alofH, n) => 
        switch(alofH) {
        | [] => n
        | [hd] when (hd <= n) => hd
        | [hd] => n
        | [hd, ...tl] => 
            if(hd <= List.hd(tl)) {
              minFIter(tl, hd)
            } else {
              minFIter(tl, List.hd(tl))
            };
        };
      minFIter(alof, 0.0)
    };
    
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
          | ([hdM, ...tlM], [hdF, ...tlF]) when (f == hdF) => hdM
          | ([hdM, ...tlM], [hdF, ...tlF]) => bestMove(tlM, tlF, f)
          | _ => failwith("The float doesn't correspond to a move")
          };

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

    /* foldMap creates a 'c list by applying a procedure that takes in an 'a list 
        and a second element 'b to produce a 'c type */
    /* input: 
         f: a procedure with type signature ('a, 'b) => 'c, that takes in a list 
            of 'a and an element 'b
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
  
    /* input:
       output:
       */
    let minimax: PlayerGame.state => PlayerGame.move = cState => {
      /* the minimax algorithm, estimates the value of all possible next moves 
          based on estimateValue
          the algorithm determines the min value, the worst for the AI, when its
            the opponents turn, and the max value, the best for the AI, when its
            the AI's turn.  These values are propogated up to the current state
            (the root), where the max value is taken to decide the best move for
            the AI to make
            */
      /* input: 
            alomMax: a list of moves that represents possible moves to make by 
                     the AI
            alomMinL a list of moves that represents possible moves to make by 
                     the opponent
            d: the depth of the minimax algorithm
         output:
            a float that represents the best move to make based on the minimax
            algorithm
           */
      let rec max: (list(PlayerGame.move), int) => float = (alomMax, d) => 
        switch(d) {
        | 1 => maxF(
          List.map(PlayerGame.estimateValue, aLstOfStates(alomMax, cState))
          )
        | _ => maxF(
          foldMap(
            min, 
            List.map(PlayerGame.legalMoves, aLstOfStates(alomMax, cState)), 
            (d-1)
            ))
        }
      and min: (list(PlayerGame.move), int) => float = (alomMin, d) => 
        switch(d) {
        | 1 => 
          minF(
            List.map(PlayerGame.estimateValue, aLstOfStates(alomMin, cState))
            )
        | _ => minF(
          foldMap(
            max,
            List.map(PlayerGame.legalMoves, aLstOfStates(alomMin, cState)), 
            (d-1)
            ))
        };
 
      let posMoves = PlayerGame.legalMoves(s);
      bestMove()
      
      };
    };
  
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

