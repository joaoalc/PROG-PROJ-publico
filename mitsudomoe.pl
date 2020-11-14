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
    inputType(Arg1, Arg2, Arg3),
    % format('~n ~p ~p ~p ', [Piece, Line,Col]),
    playPiece(Board, Arg1, Arg2, Arg3, UpdatedBoard).   

/*GAME LOOP ---------------------------------------------*/
gameLoop(_,1) :- getPlayerName(1,Name), format('~n Congrats ~s, you win!!', Name), !.
gameLoop(_,2) :- getPlayerName(1,Name), format('~n Congrats ~s, you win!!', Name), !.
gameLoop(Board, Winner) :-
    getPlayerTurn(PlayerID, 1), % get current player
    displayGame(Board, PlayerID), % display result
    playerTurn(PlayerID, Board, UpdatedBoard), %execute turn
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
    displayBoard(GameState),
    getStashSize(PlayerID, Size),
    format('~n Stash: ~p rings ~n', Size). %TODO define ring stash

printHeader(PlayerID) :-
    getPlayerName(PlayerID, Name),
    getPlayerColor(PlayerID, Color),
    nl,
    write('======================================='),
    format('~n Player: ~p   ', Name),
    format(' Color: ~p ~n', Color),
    write('=======================================').


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
    write('nice2'),
    playCol(M, MoreCols, Piece, NewPieces).
