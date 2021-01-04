:-use_module(library(clpfd)).
:-use_module(library(lists)).

:-consult('restrictions.pl').
:-consult('utils.pl').
:-consult('boardRandomization.pl').

lightBulb :- 
   once(testBoard(NBoard)),
   checkBoard(NBoard),           % check if the input Board is valid
   statistics(walltime, [Start|_]), % start statiscs
   bagof(ResultBoard, lightbulb(NBoard, ResultBoard), List), % find all the possible solutions
   statistics(walltime, [End|_]), % stop counting
   Elapsed is End - Start,
   displayResults(List, NBoard, Elapsed).

lightBulb :- % in case there are no possible results
   write('No results found!').

% read board from file
lightBulbFile :-
   readFromFile('boards.txt', Board),
   statistics(walltime, [Start|_]), % start statiscs
   bagof(ResultBoard, lightbulb(Board, ResultBoard),  List),
   statistics(walltime, [End|_]), % stop counting
   Elapsed is End - Start,
   displayResults(List, Board, Elapsed).

/* TEST BOARDS ------------------------------- */
testBoard2x2([[4,3], 
              [3,3]]). 

testBoard3x3([[3, 2, 3], 
          [3, 4, 5],
          [2, 3, 4]]). 
          
testBoard4x4([[2, 4, 4, 3],
           [4, 3, 6, 4], 
           [4, 8, 6, 6], 
           [2, 3, 4, 3]]). %First example

testBoard5x5([[3, 3, 5, 2, 3],
            [4, 6, 3, 3, 4], 
            [2, 3, 5, 5, 2], 
            [5, 4, 6, 4, 3], 
            [3, 4, 4, 4, 3]]).

% testBoard([[2, 4, 4, 2],
%            [4, 6, 4, 3], 
%            [6,5,5,4], 
%            [4,4,4,4]]).

testBoard([[4, 4, 4, 3],
            [2,4,4,5], 
            [3,5,6,4], 
            [3,4,4,2]]).

lightbulb(NumbersBoard, ResultBoard) :-
   
   length(NumbersBoard, Collen),
   getRowLength(NumbersBoard, RowLen),
   
   % variable declaration
   length(ResultBoard, Collen),
   createMatrix(ResultBoard, RowLen),
   flatten(ResultBoard, FlattenedResults), %Get the flattened lists, since it's easier and more efficient to work with them
   flatten(NumbersBoard, FlattenedNumbers), %Get the flattened lists, since it's easier and more efficient to work with them
   domain(FlattenedResults, 0, 1), %1 is lit, 0 is unlit

   % applying restrictions
   sum(FlattenedResults, #\=, 0), %Exclude all zeros
        
   restrictSpot(FlattenedNumbers, FlattenedResults, Collen, RowLen, 1),

   % solution search
   labeling([], FlattenedResults).

/* RESULT PRESENTATION --------------------------------------------------*/
displayResults(List, NBoard, Elapsed) :-
   write('ORIGINAL -------'), nl,
   printMatrix(NBoard),
   length(NBoard, BoardSize),
   showResults(List, NBoard, BoardSize),
   length(List, N),
   format('~n~n [!] Number of solutions: ~p~n', [N]),
   format('~n [!] Elapsed time: ~p~n', [Elapsed]).

showResults([], _, _).
showResults([First|Rest], Original, N) :-
   separator(N),
   showResult(Original, First, N),
   showResults(Rest, Original, N).

showResult([], [], _).
showResult([OriginalHead | OriginalRest], [OnHead | OnRest], N) :-
   nl, write('| '), showResultLine(OriginalHead, OnHead),
   separator(N),
   showResult(OriginalRest, OnRest, N).

showResultLine([], []).
showResultLine([FirstOriginal | RestOriginal], [FirstRes | RestRes]) :-
   FirstRes =:= 1,
   format('~p | ', [FirstOriginal]),
   showResultLine(RestOriginal, RestRes).

showResultLine( [_ | RestOriginal], [_|RestRes]) :-
   write('  | '),
   showResultLine(RestOriginal, RestRes).

/* INPUT BOARD VALIDATION ------------------------------------ */
% checking if all the numbers inserted in a board are valid
checkBoard([]).
checkBoard([Line | Rest]) :-
   checkLine(Line),
   checkBoard(Rest).

checkBoard(_) :-
   nl, write('Found one or more invalid numbers!'), nl, fail.

checkLine([]).
checkLine([First | Rest]) :-
   number(First),
   First > 0,     % each number must be greatter or equal to 1
   First < 10,    % each number must be less than 10
   checkLine(Rest).
