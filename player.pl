
:- dynamic player/5.
player(ID, Name, PlayerColor, Stash, PlayerTurn).

% id 3 and 4 are reserved to bot players
isBot(3).
isBot(4).

% initialize Player vs Player game
initPlayersPvP :-
    inputString('Player #1: ', Name1),
    inputString('Player #2: ',Name2),
    retractall(player(_,_,_,_,_)),
    asserta(player(1, Name1, white, 5, 1)),
    asserta(player(2, Name2, black, 5, 0)).

% init player vs bot
initPlayersPvB :-
    inputString('Player: ', Name1),
    retractall(player(_,_,_,_,_)),
    asserta(player(1, Name1, white, 5, 1)),
    asserta(player(3, 'BOT', black, 5, 0)).

initPlayersBvB :-
    retractall(player(_,_,_,_,_)),
    asserta(player(3, 'BOT#1', white, 5, 1)),
    asserta(player(4, 'BOT#2', black, 5, 0)).


% initialize Player vs Bot
initPvB :-
    inputString('Player: ', Name),
    retractall(player(_,_,_,_,_)),
    asserta(player(1, Name, white, 5, 1)),
    asserta(player(3, 'BOT', black, 5, 0)).

% switch from current player to the next
setNextPlayer :- 
    player(NextID, NextName, NextColor, NextStash, 0), % get next player
    player(CurrID, CurrName, CurrColor, CurrStash, 1), % get current player
    retractall(player(_,_,_,_,_)),
    asserta(player(CurrID, CurrName, CurrColor, CurrStash, 0)), % set previous player turn to 0
    asserta(player(NextID, NextName, NextColor, NextStash, 1)). % set next player turn to 1


decrementRingStash :-
    player(CurrID, CurrName, CurrColor, CurrStash, 1), % get current player
    \+isBot(CurrID),
    write('new'), nl, write(CurrID),
    retract(player(CurrID, _, _, _, _)),
    NewSize is CurrStash-1,
    asserta(player(CurrID, CurrName, CurrColor, NewSize, 1)).

decrementRingStash.

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
selectPiece('B', white, wb).
selectPiece('B', black, bb).

selectRing(white, wr).
selectRing(black, br).
selectBall(white, wb).
selectBall(black, bb).


% get input type to call respective inputMove Funtion
% values of inputMove are returned in Arg1, Arg2

inputType(Color, Ret) :- 
    repeat,
        once(inputString('Type: ', Type)),
        inputMove(Type, Color, Ret).

% for second moves
inputBallMove(Color, Ret) :-
    nl, write('Choose the ball you wish to displace'), nl,
    inputMove('MB2', Color, Ret).

inputListIndex(Length, Index) :-
   once(inputString('Index (starts at 0): ', Index)),
   Index < Length.

inputListIndex(_, _) :- nl, write('[X] Invalid list index'), nl, fail.

    
inputCoords2(Line, Col) :-
    inputString('Line (A-E): ', L), !,
    \+number(L),
    L \== '',
    char_code(L, Code),
    Line is Code-65,             % starts at 64+1
    Line >= 0, Line =< 4,
    inputString('Col (1-5):  ', C), !,
    number(C),
    Col is C-1.

inputCoords4(SrcLine, SrcCol, DestLine,DestCol) :-
    inputString('Source Line (A-E): ', L1), !,
    \+number(L1),
    L1 \== '',
    char_code(L1, Code),
    SrcLine is Code-65,             % starts at 64+1
    inputString('Source Col (1-5):  ', C1), !,
    number(C1),
    SrcCol is C1-1,
    inputString('Dest Line (A-E): ', L2), !,
    \+number(L2),
    L2 \== '',
    char_code(L2, Code2),
    DestLine is Code2-65,             % starts at 64+1
    inputString('Dest Col (1-5):  ', C2), !,
    number(C2),
    DestCol is C2-1.


% Place ring from stash
inputMove('R', Color, ['R', Color, Line, Col]) :-
    inputCoords2(Line, Col).



% move top element from one cell to the other
inputMove('MR', Color, ['MR', Color, Line1, Col1, Line2, Col2]) :-
    inputCoords4(Line1, Col1, Line2, Col2).

inputMove('MB', Color, ['MB', Color, Line1, Col1, Line2, Col2]) :-
    inputCoords4(Line1, Col1, Line2, Col2).

% input for second moves
inputMove('MB2', Color, ['MB', Color, Line1, Col1]) :-
    inputCoords2(Line1, Col1).

% input for ball relocation
inputMove('RB', _, [Line1, Col1]) :-
    inputCoords2(Line1, Col1).

inputMove(_,_,_) :-
    nl, write('[X] Invalid input (inputMove)'), nl, fail.








