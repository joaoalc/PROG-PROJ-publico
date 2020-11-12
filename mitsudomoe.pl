:- prolog_flag(single_var_warnings,_,off).
:-consult('board.pl').
:-consult('player.pl').


play :- 
    initPlayersPvP, % initialize players
    initial(Board), % initizlize board
    gameLoop(Board). % start game loop

% /*Testing*/
% doIt(Board) :-
%     playPiece(Board, 1,1,wr,Res),
%     displayGame(Res, 'TESTING ').

playerTurn(Player, Board, UpdatedBoard) :- 
    input('Move', X),
    UpdatedBoard = Board. %TODO

/*GAME LOOP ---------------------------------------------*/
gameLoop(Board) :-
    getPlayerTurn(PlayerID, 1), % get current player
    playerTurn(PlayerID, Board, UpdatedBoard), %execute turn
    displayGame(UpdatedBoard, PlayerID), % display result
    getTopXY(Board, 0, 2, Piece),
    \+endGame(Board, PlayerID),
    setNextPlayer, % switch to next player
    gameLoop(Board).

/*END GAME ----------------------------------------------*/
endGame(Board, PlayerID) :-
    getPlayerColor(PlayerId, Color),
    isEndGame(Board, Color),
    !,
    getPlayerName(PlayerID, Name),
    format('[!] ~p is the winner', Name).

isEndGame(Board, white) :- whiteEndGame(Board).
isEndGame(Board, black) :- blackEndGame(Board).

% check for white victory
% true when left bottom corners have all white balls
whiteEndGame(Board) :-
    getTopXY(Board, 0, 3, D1),  
    getTopXY(Board, 0, 4, E1),
    getTopXY(Board, 1, 4, E2),
    equalTo(D1, wb),
    equalTo(E1, wb),
    equalTo(E2, wb).

% check for black victory
% true when right top corners have all black balls
blackEndGame(Board) :-
    getTopXY(Board, 3, 0, A4),  
    getTopXY(Board, 4, 0, A5),
    getTopXY(Board, 4, 1, B5),
    equalTo(A4, bb),
    equalTo(A5, bb),
    equalTo(B5, bb).

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
