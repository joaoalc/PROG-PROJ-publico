:- prolog_flag(single_var_warnings,_,off).
:-consult('utils.pl').
:-consult('board.pl').
:-consult('validation.pl').
:-consult('player.pl').
:-consult('move.pl').
:-consult('bot.pl').
:-consult('evaluateBoard.pl').
:-consult('menu.pl').

play :- 
    printMenu,
    selectOption(N),
    write('\33\[2J'),   % clear Screen
    startGame(N).


% startGame(+Option)
% start game Multiplayer
startGame(0) :-
    initPlayersPvP,
    initial(Board),
    gameLoop(Board, _, 0).

% start game Singleplayer
startGame(1) :-
    selectLevel(Lvl),
    initPlayersPvB,
    initial(Board),
    gameLoop(Board, Lvl, 0).

% start game Computer vs Computer
startGame(2) :-
    selectLevel(Lvl),
    initPlayersBvB,
    initial(Board),
    gameLoopBvB(Board, Lvl, 2).

startGame(3). % quit

       
        
/* MOVE -----------------------------------------------------------*/
% move(+GameState, +Move, -NewGameState)
% validates and executes a movement
move(GameState, Move, NewGameState) :-
    once(getNth(0, Move, Type)),
    isValidMove(GameState, Move),
    executeMove(Type, GameState, Move, NewGameState).

% executeMove(+Type, +GameState, +Move, -NewGameState)
% move top ball from A to B
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
% gameLoop(+GameState, +Lvl, -Winner)
% player vs player
gameLoop(_,_,1) :- getPlayerName(1,Name), format('~n Congrats ~s, you win!!', Name), !.
gameLoop(_,_,2) :- getPlayerName(2,Name), format('~n Congrats ~s, you win!!', Name), !.
gameLoop(_,_,3) :- getPlayerName(3,Name), format('~n Congrats ~s, you win!!', Name), !.
gameLoop(_,_,4) :- getPlayerName(4,Name), format('~n Congrats ~s, you win!!', Name), !.
gameLoop(Board, Lvl, Winner) :-
    getPlayerTurn(PlayerID, 1), % get current player
    executeTurn(Board, PlayerID,Lvl, UpdatedBoard),
    gameOver(UpdatedBoard, Winner), % check for game over
    setNextPlayer, % switch to next player
    gameLoop(UpdatedBoard, Lvl, Winner).

% bot vs bot
gameLoopBvB(_,_,3) :- getPlayerName(3,Name), format('~n Congrats ~s, you win!!', Name), !.
gameLoopBvB(_,_,4) :- getPlayerName(4,Name), format('~n Congrats ~s, you win!!', Name), !.
gameLoopBvB(Board, Lvl, _) :-
    getPlayerTurn(BotID, 1), % get current player
    executeTurn(Board, BotID, Lvl, UpdatedBoard),
    gameOver(UpdatedBoard, Winner),
    setNextPlayer, % switch to next player
    gameLoopBvB(UpdatedBoard, Lvl, Winner).

% executeTurn(+GameState, +PlayerID, +Lvl, -NewGameState)
% execute bot turn
executeTurn(Board, BotID, Lvl, UpdatedBoard) :-
    isBot(BotID),
    chooseMove(Board, BotID, Lvl, UpdatedBoard),
    displayGame(UpdatedBoard, BotID).

% execute player turn
executeTurn(Board, PlayerID, _, UpdatedBoard) :-
    displayGame(Board, PlayerID), !,
    executePlayerTurn(Board, PlayerID, UpdatedBoard).


/*END GAME ----------------------------------------------*/
% gameOver(+GameState, -Winner)
% asserts if the game has been won
gameOver(Board, Winner) :-
    isEndGame(Board, Value),
    Winner is Value. % game over

gameOver(_, 0) :- nl, write('[!] Next Turn'), nl.  % game continues
    
% isEndGame(+GameState, -Winner)
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
% displayGame(+GameState, +Player)
% print board
displayGame(GameState, PlayerID) :-  
    printHeader(PlayerID),
    displayBoard(GameState).

% print player information
printHeader(PlayerID) :-
    getPlayerName(PlayerID, Name),
    getPlayerColor(PlayerID, Color),
    getStashSize(PlayerID, Size), % get available rings
    nl,
    write('======================================='),
    format('~n Player: ~p   ', Name),
    format('Color: ~p ', Color),
    format('~n Rings:  ~p ~n', Size),
    write('=======================================').

/* MOVE PIECE ---------------------------------------------*/  
% movePiece(+GameState, +SourceLine, +SourceCol, +DestLine, +DestCol, -NewGameState)
% moves a piece from (Xsrc, Ysrc) to (Xdest, Ydest)
movePiece(BoardIn, Ysrc, Xsrc, Ydest, Xdest, BoardOut) :- 
    popTopXY(BoardIn,Ysrc, Xsrc,  TmpB, Piece),
    playPiece(TmpB, Ydest, Xdest, Piece, BoardOut).


/*PIECE PLACEMENT----------------------------------------*/
% playPiece(+GameState, +Line, +Col, +Piece, -NewGameState)
% plays a given piece on (Col, Line)
playPiece(BoardIn, Line, Col, Piece, BoardOut) :-                    
    playLine(Line, BoardIn, Col, Piece, BoardOut).

playLine(0, [Line | Rest], Col, Piece, [NewLine | Rest]) :- 
    playCol(Col, Line, Piece, NewLine). 

playLine(Lindex, [Line | Rest], Col , Piece, [Line | NewLines]) :- 
    M is Lindex-1,
    playLine(M, Rest, Col, Piece, NewLines).

% push element to the top of the stack
pushFront(Piece, List, [Piece|List]).

playCol(0, [Stack | MoreStacks], Piece, [Res|MoreStacks]) :-
    pushFront(Piece, Stack, Res).

playCol(N, [P | MoreCols], Piece, [P | NewPieces]) :-
    M is N-1, N > 0,
    playCol(M, MoreCols, Piece, NewPieces).


