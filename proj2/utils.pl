% Flattens a list by one-level
flatten([], []).

flatten([A|B],L) :- 
    is_list(A),
    flatten(B,B1), 
    !,
    append(A,B1,L).

flatten([A|B], [A|B1]) :- 
    flatten(B, B1).


%Move to utils
getRowLength([First|_], RowLen) :-
   length(First, RowLen).

%Creates a matrix of variables from a list of variables. Each list element is now a list of Ncols anonymous variables
createMatrix([], _).

createMatrix([List|Rest], NCols) :-
   length(List, NCols),
   createMatrix(Rest, NCols).

% printLine(+Line)
% prints board row
printLine([]) :- nl.
printLine([Head|Tail]) :-
    format('~p | ', [Head]),
    printLine(Tail).

% printMatrix(+Board)
% prints board matrix
printMatrix([]).
printMatrix([Head|Tail]) :-
    write('| '),
    printLine(Head),
    printMatrix(Tail).

separatorAux(2).
separatorAux(N) :- N1 is N-1,
                   write('----'),
                   separatorAux(N1).

separator(N) :-
    nl, write('+----'),
    separatorAux(N),
    write('---+').

writeToFile(File) :-
    open(File, write, Stream),
    format(Stream, "np.nm", [feup]),
    close(Stream).

readFromFile(File, Board) :-
    open(File, read, Stream),
    read(Stream, Board),
    close(Stream).