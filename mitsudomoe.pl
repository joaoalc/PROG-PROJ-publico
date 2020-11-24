:- prolog_flag(single_var_warnings,_,off).
:-consult('utils.pl').
:-consult('board.pl').
:-consult('validation.pl').
:-consult('player.pl').
:-consult('move.pl').
:-consult('bot.pl').


play :- 
    write('\33\[2J'),   % clear Screen
    initPlayersPvP, % initialize players
    initial(Board) ,!, % initialize board
    gameLoop(Board, 0). % start game loop

% use this to debug functions
test :- 
    initPlayersPvP, % initialize players
    initial2(Board), % initialize board
    gameOver(Board, Winner),
    write(Winner).

test2 :- 
    isLinearMove(4,0,2,2).
                    
test3 :-
    initial2(Board),
    getPossiblePlays(Board, Boards, white).


/* EXECUTE TURN ---------------------------------------*/
% executeTurn(Player, Board, UpdatedBoard)
/*
executeTurn(Player, Board, UpdatedBoard) :- 
    getPlayerColor(Player, Color),
    repeat,
       once(inputType(Color, Move)),
       move(Board, Move, UpdatedBoard).
       */
       
        

move(GameState, Move, NewGameState) :-
    once(getNth(0, Move, Type)),
    isValidMove(GameState, Move),
    executeMove(Type, GameState, Move, NewGameState).



/*
move(GameState, Move, NewGameState) :-
    getNth(0, Move, Type),
    % TODO verify valid move
    executeMove(Type, GameState, Move, NewGameState).*/

executeMoveAI('R',GameState, Move, NewGameState) :-
    getNth(1, Move, Color),     % get player's color
    selectRing(Color, Ring),    % select respctive ring
    getNth(2, Move, Line),
    getNth(3, Move, Col),
    %format('~n ~p ~p  ', [Col, Line]),
    playPiece(GameState, Line, Col, Ring, NewGameState).


% move top piece from A to B
executeMoveAI('MB',GameState, [_,_,Ysrc,Xsrc,Ydest,Xdest], NewGameState) :-
    % format('~n y ~p  x ~p | y ~p x ~p ', [ Ysrc,Xsrc, Ydest, Xdest]),
    movePiece(GameState, Ysrc,Xsrc, Ydest, Xdest,  NewGameState).

/*
% move top piece from A to B
executeMoveAI('MB',GameState, Move, NewGameState) :-
    getNth(2, Move, Ysrc),
    getNth(3, Move, Xsrc),
    getNth(4, Move, Ydest),
    getNth(5, Move, Xdest),
    % format('~n y ~p  x ~p | y ~p x ~p ', [ Ysrc,Xsrc, Ydest, Xdest]),
    movePiece(GameState, Ysrc,Xsrc, Ydest, Xdest,  NewGameState).
*/

executeMoveAI('MR',GameState, Move, NewGameState) :-
    getNth(2, Move, Ysrc),
    getNth(3, Move, Xsrc),
    getNth(4, Move, Ydest),
    getNth(5, Move, Xdest),
    % format('~n y ~p  x ~p | y ~p x ~p ', [ Ysrc,Xsrc, Ydest, Xdest]),
    movePiece(GameState, Ysrc,Xsrc, Ydest, Xdest,  NewGameState).
/*
% relocate ball on vaulting operation
executeMoveAI('RB', GameState, Move, NewGameState) :-
    getNth(2, Move, SrcLine),
    getNth(3, Move, SrcCol),
    getNth(4, Move, DestLine),
    getNth(5, Move, DestCol),
    movePiece(GameState, SrcLine, SrcCol, DestLine, DestCol, NewGameState).
*/
% move top piece from A to B
executeMove('MB',GameState, [_,_,Ysrc,Xsrc,Ydest,Xdest], NewGameState) :-
    % format('~n y ~p  x ~p | y ~p x ~p ', [ Ysrc,Xsrc, Ydest, Xdest]),
    movePiece(GameState, Ysrc,Xsrc, Ydest, Xdest,  NewGameState).

% place ring
executeMove('R',GameState, [_,Color,Line,Col], NewGameState) :-
    selectRing(Color, Ring),    % select respctive ring
    % format('~n ~p ~p  ', [Col, Line]),
    playPiece(GameState, Line, Col, Ring, NewGameState),
    decrementRingStash.

% move top piece from A to B
executeMove('MB',GameState, [_,_,Ysrc,Xsrc,Ydest,Xdest], NewGameState) :-
    % format('~n y ~p  x ~p | y ~p x ~p ', [ Ysrc,Xsrc, Ydest, Xdest]),
    movePiece(GameState, Ysrc,Xsrc, Ydest, Xdest,  NewGameState).

executeMove('MR',GameState, [_,_,Ysrc,Xsrc,Ydest,Xdest], NewGameState) :-
    % format('~n y ~p  x ~p | y ~p x ~p ', [ Ysrc,Xsrc, Ydest, Xdest]),
    movePiece(GameState, Ysrc,Xsrc, Ydest, Xdest,  NewGameState).

% relocate ball on vaulting operation
executeMove('RB', GameState, [_,_,SrcLine,SrcCol,DestLine,DestCol], NewGameState) :-
    movePiece(GameState, SrcLine, SrcCol, DestLine, DestCol, NewGameState).



/*GAME LOOP ---------------------------------------------*/
gameLoop(_,1) :- getPlayerName(1,Name), format('~n Congrats ~s, you win!!', Name), !.
gameLoop(_,2) :- getPlayerName(1,Name), format('~n Congrats ~s, you win!!', Name), !.
gameLoop(Board, _) :-
    getPlayerTurn(PlayerID, 1), % get current player
    displayGame(Board, PlayerID), !, % display result
    once(executePlayerTurn(Board, PlayerID, UpdatedBoard)), % execute turn for current player
    gameOver(UpdatedBoard, Value),
    setNextPlayer, % switch to next player
    gameLoop(UpdatedBoard, Value).

/*END GAME ----------------------------------------------*/
% asserts if the game has been won
gameOver(Board, Winner) :-
    isEndGame(Board, Value) -> 
        Winner is Value % game over
        ; 
        Winner is 0, nl, write('[!] Next Turn'), nl.  % game continues
    

% verify is white or black pieces have reached their goals
isEndGame(Board, Winner) :- 
    % true when left bottom corners have all white balls (white pieces win)
    once((
        getTopXY(Board, 0, 3, D1),  
        getTopXY(Board, 0, 4, E1),
        getTopXY(Board, 1, 4, E2))
    ),
    D1 == wb,
    E1 == wb,
    E2 == wb,
    getPlayerColor(Winner, white).

% true when right top corners have all black balls (black pieces win)
isEndGame(Board, Winner) :- 
    once((
        getTopXY(Board, 3, 0, A4),  
        getTopXY(Board, 4, 0, A5),
        getTopXY(Board, 4, 1, B5))
    ),
    A4 == bb,
    A5 == bb, 
    B5 == bb, 
    getPlayerColor(Winner, black).

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
movePiece(BoardIn, Ysrc, Xsrc, Ydest, Xdest, BoardOut) :- 
    popTopXY(BoardIn,Ysrc, Xsrc,  TmpB, Piece),
    playPiece(TmpB, Ydest, Xdest, Piece, BoardOut).


/*PIECE PLACEMENT----------------------------------------*/
playPiece(BoardIn, Line, Col, Piece, BoardOut) :-                      %TODO line indexes are letters
    playLine(Line, BoardIn, Col, Piece, BoardOut).

 %TabOut=tabuleiro com  a peça
playLine(0, [Line | Rest], Col, Piece, [NewLine | Rest]) :- 
    playCol(Col, Line, Piece, NewLine). 

playLine(Lindex, [Line | Rest], Col , Piece, [Line | NewLines]) :- 
    M is Lindex-1,
    playLine(M, Rest, Col, Piece, NewLines).

%M is N -1

pushFront(Piece, List, [Piece|List]).

playCol(0, [Stack | MoreStacks], Piece, [Res|MoreStacks]) :-
    %playableOn(Piece, Stack), %playableOn only works if it's supposed to place a piece there. If it isn't, (probably backtracking issues) cause it to crash
    pushFront(Piece, Stack, Res).

playCol(N, [P | MoreCols], Piece, [P | NewPieces]) :-
    M is N-1, N > 0,
    playCol(M, MoreCols, Piece, NewPieces).


