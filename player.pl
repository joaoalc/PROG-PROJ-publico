
:- dynamic player/5.
player(ID, Name, PlayerColor, stash, playerTurn).

% initialize Player vs Player game
initPlayersPvP :-
    inputString('Player #1: ', Name1),
    inputString('Player #2: ',Name2),
    retractall(player(_,_,_,_)),
    asserta(player(1, Name1, white, 5, 1)),
    asserta(player(2, Name2, black, 5, 0)).

% switch from current player to the next
setNextPlayer :- 
    player(NextID, NextName, NextColor, NextStash, 0), % get next player
    player(CurrID, CurrName, CurrColor, CurrStash, 1), % get current player
    retractall(player(_,_,_,_,_)),
    asserta(player(CurrID, CurrName, CurrColor, CurrStash, 0)), % set previous player turn to 0
    asserta(player(NextID, NextName, NextColor, NextStash, 1)). % set next player turn to 1



% GETTERS ------------------------------------------
getPlayerName(ID, Name) :- player(ID, Name,_,_,_).

getPlayerColor(ID, Color) :- player(ID,_, Color,_,_).

getPlayerTurn(ID, Turn) :- player(ID,_,_,_,Turn). % get Turn for player with ID || get playerID with Turn (0 | 1)

getStashSize(ID, Size) :- player(ID,_,_,Size,_).    % TODO decrement stash


% PLAYER INPUT ------------------------------------------
inputString(Msg , X) :-
    format('~n ~s', [Msg]),
    get_code(C),
    readRest(C,Asciis),
    name(X,Asciis).

readRest(10,[]).
readRest(13,[]).
readRest(C,[C|Rest]) :- 
    get_code(C2),
    readRest(C2,Rest).


% PARSE MOVE --------------------------------------

% select piece according to player's color 
selectPiece('R', white, wr).
selectPiece('R', black, br).

% get input type to call respective inputMove Funtion
% values of inputMove are returned in Arg1, Arg2

inputType(Ret) :-    %TODO change return to list
    inputString('Type: ', Type),
    !,
    inputMove(Type, Ret).


% Place ring from stash
inputMove('R', ['R', Piece, Line, Col]) :-
    getPlayerTurn(ID, 1),
    getPlayerColor(ID, Color),
    selectPiece('R', Color, Piece),
    inputString('Line: ', L),
    char_code(L, Code),
    Line is Code-65,             % starts at 64+1
    inputString('Col:  ', C),
    Col is C-1.

% move top element from one cell to the other
inputMove('M', ['M', Line1, Col1, Line2, Col2]) :-
    inputString('Line 1 (A-E): ', L1),
    char_code(L1, Code),
    Line1 is Code-65,             % starts at 64+1
    inputString('Col1 (1-5):  ', C1),
    Col1 is C1-1,
    inputString('Line2 (A-E): ', L2),
    char_code(L2, Code2),
    Line2 is Code2-65,             % starts at 64+1
    inputString('Col2 (1-5):  ', C2),
    Col2 is C2-1.








