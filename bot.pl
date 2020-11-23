:- use_module(library(between)).
:- use_module(library(random)).

temp(Line, Board, Move, NewGameState) :-
    getNth(0, Move, Type),
    isValidMove(Board, Move),
    executeMoveTemp(Type, Board, Move, NewGameState).

getPossiblePlays(Board, AllBoards, Color) :-
    findall(NewGameState, (between(0, 4, Line), between(0, 4, Col), temp(Line, Board, ['R', Color, Line, Col], NewGameState)), AllBoards),
    random_member(ChosenBoard, AllBoards),
    write(ChosenBoard).


