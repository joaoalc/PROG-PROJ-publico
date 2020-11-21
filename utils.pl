% Get nth element in list
getNth(0, [Head|_], Head).
getNth(N, [_|Tail], Ret) :-
    N > -1,
    N1 is N-1,
    getNth(N1, Tail, Ret).

% select Row at index Y
selectLine([First|_], 0, First).    
selectLine([First|Rest], Y, Ret) :-
    Y1 is Y-1,
    selectLine(Rest,Y1, Ret).

% select a cell at index X
selectCell([Head|_], 0, Head).
selectCell([Head|Tail], X, Cell) :-
    X1 is X-1,
    selectCell(Tail, X1, Cell).

% delete element at index N
deleteNth(0, [_|Tail], Tail).
deleteNth( N, [Head|Tail],[Head|Ret]) :-
    N1 is N-1,
    deleteNth(N1, Tail, Ret).

% replace an element at index I
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

popTop([Head|Tail], Tail, Head).
pushTop(Elem, List, [Elem|List]).
