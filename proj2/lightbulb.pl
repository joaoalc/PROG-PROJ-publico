:-use_module(library(clpfd)).
:-use_module(library(lists)).

:-consult('restrictions.pl').
:-consult('utils.pl').

a(A + B) :-
   A + B is 3,
   write(A), 
   write(B),
   fail.
a(_, _).

lb(ResultBoard) :- 
   testBoard(NBoard),
   %lightbulb(NBoard, ResultBoard).
   setof(ResultBoard, lightbulb(NBoard, ResultBoard), List),
   writethings(List).

lb(_) :-
   write('No results found!').

writethings([]).
writethings([First|List]) :-
   write(First), nl,
   writethings(List).

%testBoard([[1, 1, 1], [1, 1, 1]]). %Example where no option is valid
%testBoard([[3, 3, 5, 2],[4, 6, 3, 3], [2, 3, 5, 5], [2, 4, 4, 4]]). %Solved example at the top; Has multiple solutions
testBoard([[2, 4, 4, 3],[4, 3, 6, 4], [4, 8, 6, 6], [2, 3, 4, 3]]). %First example



lightbulb(NumbersBoard, ResultBoard) :-
   length(NumbersBoard, Collen),
   getRowLength(NumbersBoard, RowLen),
   length(ResultBoard, Collen),
   createMatrix(ResultBoard, RowLen),
   flatten(ResultBoard, FlattenedResults), %Get the flattened lists, since it's easier and more efficient to work with them
   flatten(NumbersBoard, FlattenedNumbers), %Get the flattened lists, since it's easier and more efficient to work with them
   domain(FlattenedResults, 0, 1), %1 is lit, 0 is unlit
   %Temporary test code
   

   sum(FlattenedResults, #\=, 0), %Exclude all zeros
   
   restrictSpot(FlattenedNumbers, FlattenedResults, Collen, RowLen, 1),
   labeling([], FlattenedResults).
