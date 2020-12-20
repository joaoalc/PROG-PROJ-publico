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
getRowLength([First|NumbersBoard], RowLen) :-
   length(First, RowLen).

%Creates a matrix of variables from a list of variables. Each list element is now a list of Ncols anonymous variables
createMatrix([], _).

createMatrix([List|Rest], NCols) :-
   length(List, NCols),
   createMatrix(Rest, NCols).