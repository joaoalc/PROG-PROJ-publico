:- use_module(library(between)).
:- use_module(library(random)).

allBallMoves(Line, Board, Move, NewGameState) :-
    getNth(0, Move, Type),
    getNth(1, Move, Color),
    !,

    isValidBallMove(Board, Color, Move),
    executeMoveAI(Type, Board, Move, NewGameState).
    /*getNth(2, Move, LineSrc),
    getNth(3, Move, ColSrc),
    getNth(4, Move, LineDest),
    getNth(5, Move, ColDest),
    write(LineSrc),
    write(ColSrc),
    write(LineDest),
    write(ColDest),
    !,
    displayBoard(NewGameState)*/

allRingMoves(Line, Board, Move, NewGameState) :-
    getNth(0, Move, Type),
    isValidMove(Board, Move),
    executeMoveAI(Type, Board, Move, NewGameState).

findBallBallMovesAfterRing([], [], _).


findBallBallMovesAfterRing([First|Rest], [FirstRes|RestRes], Color) :-
    findall(NewBallGameState, (between(0, 4, LineSrc), between(0, 4, ColSrc), between(0, 4, LineDest), between(0, 4, ColDest), allBallMoves(Line, First, ['MB', Color, LineSrc, ColSrc, LineDest, ColDest], NewBallGameState)), FirstRes),
    nl, nl,
    write('A'),
    write(FirstRes),
    nl, nl,
    findBallBallMovesAfterRing(Rest, RestRes, Color),
    write(FirstRes),
    nl, 
    nl.





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

    %get every possible ring placement.
    %findall(NewGameState, (between(0, 4, Line), between(0, 4, Col), allRingMoves(Line, Board, ['R', Color, Line, Col], NewGameState)), AllBoardsRingPlace),
    %get every possible ball movement
    %findall(NewBallGameState, (between(0, 4, LineSrc), between(0, 4, ColSrc), between(0, 4, LineDest), between(0, 4, ColDest), allBallMoves(Line, Board, ['MB', Color, LineSrc, ColSrc, LineDest, ColDest], NewBallGameState)), AllBoardsMoveBall),
    %temp(Line, Board, ['MB', Color, 0, 3, 0, 2], NewGameState),
    %write(AllBoardsMoveBall),
    findall(NewGameState, (between(0, 4, Line), between(0, 4, Col), allRingMoves(Line, Board, ['R', Color, Line, Col], NewGameState)), AllBoardsRingPlace2),
    write(AllBoardsRingPlace2),
    nl,
    findBallBallMovesAfterRing(AllBoardsRingPlace2, AllBoardsRingPlaceBallMove, Color),
    %%write(AllBoardsRingPlaceBallMove),
    %write(AllBoardsRingPlaceBallMove),
    /*flatten([AllBoardsRingPlace, AllBoardsMoveBall], EveryBoard),
    random_member(ChosenBoard, EveryBoard),
    displayBoard(ChosenBoard).*/
    nl, nl,
    write('Final Board'),
    nl, nl,
    flatten(AllBoardsRingPlaceBallMove, Result),
    write(Result),
    random_member(FinalBoard, Result),
    nl,
    displayBoard(FinalBoard).


test4 :-
    testBoard(Board),
    isValidBallMove(Board, white, ['MB', white, 0, 4, 0, 2]).