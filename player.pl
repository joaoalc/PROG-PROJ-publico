
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
    format('~n ~s ~n', [Msg]),
    get_code(C),
    readRest(C,Asciis),
    name(X,Asciis).

readRest(10,[]).
readRest(13,[]).
readRest(C,[C|Rest]) :- 
    get_code(C2), readRest(C2,Rest).


% INPUT VALIDATION --------------------------------------
