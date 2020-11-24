:- use_module(library(between)).
:- use_module(library(random)).

% moves balls for AI
executeBallMove(In, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], Out) :-
    once(isValidBallMove(In, Color, [_,_,SrcLine,SrcCol,DestLine,DestCol])),
    moveBallBot(In, Color, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], Out).

% move is not a vault (move ball)
moveBallBot(Board,_, Move, UpdatedBoard) :-
         once(getAllCoords(Move, SrcLine, SrcCol, DestLine, DestCol)),
        \+isVault(SrcLine, SrcCol, DestLine, DestCol),
        !, % if not a vault, list of moves is the move itself
        move(Board, Move, UpdatedBoard).

% in case it is a vault
moveBallBot(Board, Color, Move, UpdatedBoard) :-
        nl, write(' [i] Movement requires vault'),nl,
        getAllCoords(Move, SrcLine, SrcCol, DestLine, DestCol),
        fetchVaultedBalls(Board, Color, SrcLine, SrcCol, DestLine, DestCol, CoordsList),
        !,
        (length(CoordsList, 0) ->       % only call vault assistant is there are balls to displace
                (move(Board, Move, UpdatedBoard));
                (move(Board, Move, TmpBoard), % perform vault move
                executeVaultMovesBot(TmpBoard, CoordsList, Move, UpdatedBoard)) % relocate opponent's balls
        ).

% if the vaulted balls list is empty then no vault is performed
executeVaultMovesBot(UpdatedList, List, _, UpdatedList) :- length(List,0).

% the initial ball move is only called at the end of all the intermidiate vaults
executeVaultMovesBot(Board, CoordsList, Move, UpdatedBoard) :-
        once((copy_term(CoordsList, TmpList),
        getNth(0, CoordsList, First))),
        write(CoordsList),
        relocateBallsBot(Board, First, Move, TmpBoard),
        deleteNth(0, CoordsList, NewList),
        executeVaultMovesBot(TmpBoard, NewList, Move, UpdatedBoard).  

% relocates all opponent's vaulted balls
relocateBallsBot(Board, [SrcLine, SrcCol], Move, UpdatedBoard) :-
    between(0,4, DestLine), between(0,4, DestCol),
    once(getTopXY(Board, SrcCol, SrcLine, Ball)),
    isValidBallRelocation(Board, [_,Ball,_,_,DestLine,DestCol]),
    executeMove('RB', Board, [_,_,SrcLine,SrcCol,DestLine,DestCol], UpdatedBoard).



%generate second moves
moveGenerator('MB', In, Out) :-
    between(0,4,SrcLine), between(0,4,SrcCol),
    between(0,4,DestLine), between(0,4,DestCol),
    once(executeBallMove(In, ['MB', white, SrcLine, SrcCol, DestLine, DestCol], Out)).


% generate ring movements
moveGenerator('MR',In, Out) :-
    between(0,4,SrcLine), between(0,4,SrcCol),
    between(0,4,DestLine), between(0,4,DestCol),
    once(move(In, ['MR', white, SrcLine, SrcCol, DestLine, DestCol], Out)).

% % generate ring placement moves
moveGenerator('R',In, Out) :-
    between(0,4,Line), between(0,4,Col),
    once(move(In, ['R', white, Line, Col], Out)).

% %generate moves with 
moveGenerator('SM',In, Out) :-
    between(0,4,Line), between(0,4,Col),
    once(move(In, ['R', white, Line, Col], Tmp)),
    between(0,4,SrcLine), between(0,4,SrcCol),
    between(0,4,DestLine), between(0,4,DestCol),
    once(move(Tmp, ['MB', white, SrcLine, SrcCol, DestLine, DestCol], Out)).


flatten([], []).

flatten([A|B],L) :- 
    is_list(A),
    flatten(B,B1), 
    !,
    append(A,B1,L).

flatten([A|B], [A|B1]) :- 
    flatten(B, B1).

%To get every possible move, you need to:
getPossiblePlays(Board, AllBoards, Color) :-
    initBot, % TODO remove this (put in ai gameLoop)
    % get all possible moves
    findall(NewGameState,   % TODO put in valid_moves predicate 
    (moveGenerator(Type,Board, NewGameState)), 
    RingBoards),
    nl,
    length(RingBoards, L),
    printAll(RingBoards, L).

printAll(Board, 0).
printAll([Head|Rest], Ind) :-
    displayBoard(Head),
    I is Ind-1,
    printAll(Rest, I).
