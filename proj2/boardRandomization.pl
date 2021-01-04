:-use_module(library(random)).

%Creates a random lightbulb puzzle. This one is purely random and doesn't guarentee a solution.
%For one that guarentees at least one solution, look for randomSuccessfulLightbulb
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
    random(2, 10, Number).

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


%Given a width and height, respectively, generates and solves a random lightbulb puzzle with at least one solution
solveRandomLightBulb(RowLen, ColLen) :- %Refactor the lightbuld function to use flattened boards; use setof here to find every solution then use statistics and such

    randomSuccessfulLightbulb(_, FlattenedNumbers, RowLen, ColLen),
    BoardSize is RowLen * ColLen,
    length(FlatRes, BoardSize),
    domain(FlatRes, 0, 1), %1 is lit, 0 is unlit
    sum(FlatRes, #\=, 0), %Exclude all zeros
    restrictSpot(FlattenedNumbers, FlatRes, RowLen, ColLen, 1),


    unflattenList(FlattenedNumbers, RowLen, ColLen, Numbers),

    %unflattenList(FlatRes, RowLen, ColLen, Results),
    statistics(walltime, [Start|_]), % start statiscs
    bagof(Results, lightbulb(Numbers, Results),  List),

    statistics(walltime, [End|_]), % stop counting
    Elapsed is End - Start,
    displayResults(List, Numbers, Elapsed).

%Given a width and height, respectively, generates a random lightbulb puzzle with at least one solution
randomSuccessfulLightbulb(FlattenedResults, FlattenedNums, RowLen, ColLen) :-
    generateVariableBoard(ResultBoard, RowLen, ColLen),
    flatten(ResultBoard, FlattenedResults),
    randomizeResultBoard(FlattenedResults),
    generateVariableBoard(NumberBoard, RowLen, ColLen),
    flatten(NumberBoard, FlattenedNums),
    restrictions(FlattenedNums, FlattenedResults, RowLen, ColLen).

randomLightbulbProblem(_, _, FlattenedBoard, RowLen, ColLen) :-
    generateVariableBoard(NumberBoard, RowLen, ColLen),
    flatten(NumberBoard, FlattenedNumbers),
    restrictions(FlattenedNumbers, FlattenedBoard, RowLen, ColLen),
    write('Flattened numbers: '), write(FlattenedNumbers), nl,
    write('Flattened board: '), write(FlattenedBoard), nl.

restrictions(FlattenedNumbers, FlattenedResults, RowLen, ColLen) :-
    domain(FlattenedNumbers, 1, 9),
    restrictSpot(FlattenedNumbers, FlattenedResults, RowLen, ColLen, 1),
    labeling([value(selRandom)], FlattenedNumbers).

selRandom(Var, _, BB0, BB1):- % seleciona valor de forma aleatória
fd_set(Var, Set), fdset_to_list(Set, List),
random_member(Value, List), % da library(random)
( first_bound(BB0, BB1), Var #= Value ;
later_bound(BB0, BB1), Var #\= Value ).


%Creates a random board with lit or unlit lightbulbs; used in creating random lightbulb puzzles with at least one solution
randomizeResultBoard([]).

randomizeResultBoard([First|FlattenedResults]) :-
    random(0, 2, First),
    randomizeResultBoard(FlattenedResults).

generateVariableBoard(ResultBoard, RowLen, Collen) :-
   length(ResultBoard, Collen),
   createMatrix(ResultBoard, RowLen).

   



