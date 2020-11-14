:- prolog_flag(single_var_warnings,_,off).
:-consult('utils.pl').
:-consult('board.pl').
:-consult('player.pl').


play :- 
    initPlayersPvP, % initialize players
    initial(Board), % initialize board
    gameLoop(Board, 0). % start game loop

/* EXECUTE TURN ---------------------------------------*/
executeTurn(Player, Board, UpdatedBoard) :- 
    inputType(Ret),
    move(Board, Ret, UpdatedBoard).

% executeTurn(Player, Board, UpdatedBoard)
    

move(GameState, Move, NewGameState) :-
    getNth(0, Move, Type),
    executeMove(Type, GameState, Move, NewGameState).


% place ring
executeMove('R',GameState, Move, NewGameState) :-
    getNth(1, Move, Piece),
    getNth(2, Move, Line),
    getNth(3, Move, Col),
    playPiece(GameState, Line, Col, Piece, NewGameState).

% move top piece from A to B
executeMove('M',GameState, Move, NewGameState) :-
    getNth(1, Move, Piece),
    getNth(2, Move, Line),
    getNth(3, Move, Col),
    playPiece(GameState, Line, Col, Piece, NewGameState).


/*GAME LOOP ---------------------------------------------*/
gameLoop(_,1) :- getPlayerName(1,Name), format('~n Congrats ~s, you win!!', Name), !.
gameLoop(_,2) :- getPlayerName(1,Name), format('~n Congrats ~s, you win!!', Name), !.
gameLoop(Board, Winner) :-
    getPlayerTurn(PlayerID, 1), % get current player
    displayGame(Board, PlayerID), !, % display result
    executeTurn(PlayerID, Board, UpdatedBoard), % execute turn for current player
    gameOver(UpdatedBoard, Value),
    setNextPlayer, % switch to next player
    gameLoop(UpdatedBoard, Value).

/*END GAME ----------------------------------------------*/
% asserts if the game has been won
gameOver(Board, Winner) :-
    value(Board, PlayerID, Winner).

value(Board, PlayerID, Value) :-
    getPlayerColor(PlayerID, Color),
    !,
    isEndGame(Board) -> 
        Value is PlayerID % game over
        ; 
        Value is 0, nl, write('[!] Next Turn'), nl.  % game continues

% verify is white or black pieces have reached their goals
isEndGame(Board) :- 
    (   % true when right top corners have all white balls (white pieces win)
        getTopXY(Board, 0, 3, D1),  
        getTopXY(Board, 0, 4, E1),
        getTopXY(Board, 1, 4, E2),
        !,
        equalTo(D1, wb),
        equalTo(E1, wb),
        equalTo(E2, wb)
    );
    (    % true when left bottom corners have all black balls (black pieces win)
        getTopXY(Board, 3, 0, A4),  
        getTopXY(Board, 4, 0, A5),
        getTopXY(Board, 4, 1, B5),
        !,
        equalTo(A4, bb),
        equalTo(A5, bb),
        equalTo(B5, bb)
    ).


/*DISPLAY GAME ------------------------------------------*/
displayGame(GameState, PlayerID) :-  
    printHeader(PlayerID),
    displayBoard(GameState).

printHeader(PlayerID) :-
    getPlayerName(PlayerID, Name),
    getPlayerColor(PlayerID, Color),
    getStashSize(PlayerID, Size),
    nl,
    write('======================================='),
    format('~n Player: ~p   ', Name),
    format('Color: ~p ', Color),
    format('~n Rings:  ~p ~n', Size),
    write('=======================================').

/* MOVE PIECE ---------------------------------------------*/

/*PIECE PLACEMENT----------------------------------------*/
playPiece(BoardIn, Line, Col, Piece, BoardOut) :-                      %TODO line indexes are letters
    playLine(Line, BoardIn, Col, Piece, BoardOut).

 %TabOut=tabuleiro com  a peça
playLine(1, [Line | Rest], Col, Piece, [NewLine | Rest]) :- 
    playCol(Col, Line, Piece, NewLine). 

playLine(Lindex, [Line | Rest], Col , Piece, [Line | NewLines]) :- 
    M is Lindex-1,
    playLine(M, Rest, Col, Piece, NewLines).

%M is N -1

pushFront(Piece, List, [Piece|List]).

playCol(1, [Stack | MoreStacks], Piece, [Res|MoreStacks]) :-
    %playableOn(Piece, Stack), %playableOn only works if it's supposed to place a piece there. If it isn't, (probably backtracking issues) cause it to crash
    pushFront(Piece, Stack, Res).

playCol(N, [P | MoreCols], Piece, [P | NewPieces]) :-
    M is N-1, N > 1,
    playCol(M, MoreCols, Piece, NewPieces).
