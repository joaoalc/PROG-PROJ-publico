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