:-use_module(library(random)).

randomLightbulb(RowLen, Collen) :-
    generateRandomBoard(RandomBoard, RowLen, Collen),
    write('Board:'),
    write(RandomBoard), nl,
    setof(ResultBoard, lightbulb(RandomBoard, ResultBoard), List),
    write('Solutions:'), nl,
    showResults(List, RandomBoard).

randomLightbulb(_, _) :-
    write('No solutions found!').

randomBoardNumber(Number) :-
    random(1, 10, Number).

generateRandomBoard(RandomBoard, RowLen, Collen) :-
    length(RandomBoard, Collen),
    createMatrix(RandomBoard, RowLen),
    randomizeMatrix(RandomBoard).

randomizeMatrix([]).

randomizeMatrix([First|RandomBoard]) :-
    randomizeLine(First),
    randomizeMatrix(RandomBoard).

randomizeLine([]).

randomizeLine([First|Line]) :-
    randomBoardNumber(First),
    randomizeLine(Line).


solveRandomSuccessfulLightbulb(RowLen, ColLen) :- %Refactor the lightbuld function to use flattened boards; use setof here to find every solution then use statistics and such
    randomSuccessfulLightbulb(FlattenedResults, FlattenedNumbers, RowLen, ColLen),
    BoardSize is RowLen * ColLen,
    length(FlatRes, BoardSize),
    domain(FlatRes, 0, 1), %1 is lit, 0 is unlit
    sum(FlatRes, #\=, 0), %Exclude all zeros
    write(FlattenedResults), write(FlattenedNumbers),
    restrictSpot(FlattenedNumbers, FlatRes, RowLen, ColLen, 1),
    write('.'),
   labeling([], FlatRes),
   write(FlatRes).

randomSuccessfulLightbulb(FlattenedResults, FlattenedNums, RowLen, ColLen) :-
    generateVariableBoard(ResultBoard, RowLen, ColLen),
    flatten(ResultBoard, FlattenedResults),
    randomizeResultBoard(FlattenedResults),
    generateVariableBoard(NumberBoard, RowLen, ColLen),
    flatten(NumberBoard, FlattenedNumbers),
    setof(FlattenedNumbers, restrictions(FlattenedNumbers, FlattenedResults, RowLen, ColLen), List),
    random_member(FlattenedNums, List).


restrictions(FlattenedNumbers, FlattenedResults, RowLen, ColLen) :-
    domain(FlattenedNumbers, 1, 9),
    restrictSpot(FlattenedNumbers, FlattenedResults, RowLen, ColLen, 1),
    labeling([], FlattenedNumbers).

randomizeResultBoard([]).

randomizeResultBoard([First|FlattenedResults]) :-
    random(0, 2, First),
    randomizeResultBoard(FlattenedResults).

generateVariableBoard(ResultBoard, RowLen, Collen) :-
   length(ResultBoard, Collen),
   createMatrix(ResultBoard, RowLen).

   



