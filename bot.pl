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
moveGenerator('MB', Color, In, Out) :-
    between(0,4,SrcLine), between(0,4,SrcCol),
    between(0,4,DestLine), between(0,4,DestCol),
    once(executeBallMove(In, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], Out)).


% generate ring movements
moveGenerator('MR', Color, In, Out) :-
    between(0,4,SrcLine), between(0,4,SrcCol),
    between(0,4,DestLine), between(0,4,DestCol),
    once(move(In, ['MR', Color, SrcLine, SrcCol, DestLine, DestCol], Out)).

% % generate ring placement moves
moveGenerator('R', Color, In, Out) :-
    between(0,4,Line), between(0,4,Col),
    once(move(In, ['R', Color, Line, Col], Out)).

% %generate moves with 
moveGenerator('SM', Color, In, Out) :-
    between(0,4,Line), between(0,4,Col),
    once(move(In, ['R', Color, Line, Col], Tmp)),
    between(0,4,SrcLine), between(0,4,SrcCol),
    between(0,4,DestLine), between(0,4,DestCol),
    once(executeBallMove(Tmp, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], Out)).

%To get every possible move, you need to:
getPossiblePlays(Board, AllBoards, Color) :-
    initBot, % TODO remove this (put in ai gameLoop)
    % get all possible moves
    findall(NewGameState,   % TODO put in valid_moves predicate 
    (moveGenerator(Type, Color, Board, NewGameState)), 
    AllBoards),
    nl,
    length(AllBoards, L),
    format('~n generated  ~p moves', L).

pickPlay(Board, UpdatedBoard, Color) :-
    getPossiblePlays(Board, AllBoards, Color),
    random_member(UpdatedBoard, AllBoards).

printAll(Board, 0).
printAll([Head|Rest], Ind) :-
    displayBoard(Head),
    I is Ind-1,
    printAll(Rest, I).



chooseMove(GameState, Player, 0, Move) :-
    getPlayerColor(Player, Color),
    getPossiblePlays(GameState, AllBoards, Color),
    calcValueBoards(AllBoards, Player, Scores),
    write(Scores),
    getBestBoards(AllBoards, Scores, Move).
    %calculateValues(AllBoards, Player, Values).
