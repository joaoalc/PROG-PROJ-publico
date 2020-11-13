:- prolog_flag(single_var_warnings,_,off).
:-consult('board.pl').
:-consult('player.pl').


play :- 
    initPlayersPvP, % initialize players
    initial(Board), % initialize board
    gameLoop(Board, 0). % start game loop

% /*Testing*/
% doIt(Board) :-
%     playPiece(Board, 1,1,wr,Res),
%     displayGame(Res, 'TESTING ').

playerTurn(Player, Board, UpdatedBoard) :- 
    input('Move', X),
    UpdatedBoard = Board. %TODO

/*GAME LOOP ---------------------------------------------*/
gameLoop(_,1) :- !.
gameLoop(_,2) :- !.
gameLoop(Board, Winner) :-
    getPlayerTurn(PlayerID, 1), % get current player
    displayGame(Board, PlayerID), % display result
    playerTurn(PlayerID, Board, UpdatedBoard), %execute turn
    gameOver(UpdatedBoard, Value),
    setNextPlayer, % switch to next player
    gameLoop(UpdatedBoard, Value).

/*END GAME ----------------------------------------------*/

gameOver(Board, Winner) :-
    value(Board, PlayerID, Winner).

value(Board, PlayerID, Value) :-
    getPlayerColor(PlayerID, Color),
    !,
    isEndGame(Board) -> 
        Value is PlayerID % game over
        ; 
        Value is 0, nl, write('[!] Next Turn'), nl.  % game continues

% assert if one of the players have won the game
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
    getPlayerName(PlayerID, Name),
    displayBoard(GameState, Name).


/*PIECE PLACEMENT----------------------------------------*/
playPiece(BoardIn, Line, Col, Piece, BoardOut) :-                        %TODO line indexes are letters
    playLine(Lindex, BoardIn, Col, Piece, BoardOut).

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
