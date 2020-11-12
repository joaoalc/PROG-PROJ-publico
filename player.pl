
:- dynamic player/4.
player(ID, Name, PlayerColor, playerTurn).

% initialize Player vs Player game
initPlayersPvP :-
    readPlayer('Player #1: ', Name1),
    readPlayer('Player #2: ',Name2),
    retractall(player(_,_,_,_)),
    asserta(player(1, Name1, white, 1)),
    asserta(player(2, Name2, black, 0)).

% switch from current player to the next
setNextPlayer :- 
    player(NextID, NextName, NextColor, 0), % get next player
    player(CurrID, CurrName, CurrColor, 1), % get current player
    retractall(player(_,_,_,_)),
    asserta(player(CurrID, CurrName, CurrColor, 0)), % set previous player turn to 0
    asserta(player(NextID, NextName, NextColor, 1)). % set next player turn to 1



% GETTERS ------------------------------------------
getPlayerName(ID, Name) :- player(ID, Name,_,_).

getPlayerColor(ID, Color) :- player(ID,_, Color,_).

getPlayerTurn(ID, Turn) :- player(ID,_,_,Turn). % get Turn for player with ID || get playerID with Turn (0 | 1)


% PLAYER NAME INPUT --------------------------------
readPlayer(Msg,X) :- 
    format('~n ~s ~n', [Msg]),
    get_code(C),
    readRest(C,Asciis),
    name(X,Asciis).

readRest(10,[]).
readRest(13,[]).
readRest(C,[C|Rest]) :- 
    get_code(C2), readRest(C2,Rest).

% MOVE INPUT ------------------------------------------
input(Msg , X) :-
    format('~n ~s ~n', [Msg]),
    get_code(C),
    readRest(C,Asciis),
    name(X,Asciis).