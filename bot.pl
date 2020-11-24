:- use_module(library(between)).
:- use_module(library(random)).

executeBallMove(Line, Board, Move, NewGameState) :-
    getNth(0, Move, Type),
    getNth(1, Move, Color),
    !,

    isValidBallMove(Board, Color, Move),
    once(executeMoveAI(Type, Board, Move, NewGameState)).
    /*getNth(2, Move, LineSrc),
    getNth(3, Move, ColSrc),
    getNth(4, Move, LineDest),
    getNth(5, Move, ColDest),
    write(LineSrc),
    write(ColSrc),
    write(LineDest),
    write(ColDest),
    nl, nl.
    
    !,
    displayBoard(NewGameState)*/

%generate second moves
moveGenerator('MB', In, Out) :-
    between(0,4,SrcLine), between(0,4,SrcCol),
    between(0,4,DestLine), between(0,4,DestCol),
    once(move(In, ['MB', white, SrcLine, SrcCol, DestLine, DestCol], Out)).


%generate ring movements
moveGenerator('MR',In, Out) :-
    between(0,4,SrcLine), between(0,4,SrcCol),
    between(0,4,DestLine), between(0,4,DestCol),
    once(move(In, ['MR', white, SrcLine, SrcCol, DestLine, DestCol], Out)).

% generate ring placement moves
moveGenerator('R',In, Out) :-
    between(0,4,Line), between(0,4,Col),
    once(move(In, ['R', white, Line, Col], Out)).

%generate moves with 
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
    initBot,
    % get all possible
    findall(NewGameState, 
    (moveGenerator(Type,Board, NewGameState)), 
    RingBoards),
    nl, nl,
    write('Final Board'),
    nl, nl,
    % flatten(AllBoardsRingPlaceBallMove, Result),
    % write(Result),
    % random_member(FinalBoard, Result),
    nl,
    length(RingBoards, L),
    printAll(RingBoards, L).

printAll(Board, 0).
printAll([Head|Rest], Ind) :-
    displayBoard(Head),
    I is Ind-1,
    printAll(Rest, I).
test4 :-
    testBoard(Board),
    isValidBallMove(Board, white, ['MB', white, 0, 4, 0, 2]).