:- use_module(library(lists)).

elem(e, C) :- C = ' '.
elem(c, C) :- C = '.'.
elem(wb, C) :- C = 'WB'.  % white Ball
elem(bb, C) :- C = 'BB'.  % black Ball
elem(wr, C) :- C = 'WR'.  % white ring
elem(br, C) :- C = 'BR'.   % black ring

boardEmpty([
    [[e],[e],[e],[c],[c]],
    [[e],[e],[e],[e],[c]],
    [[e],[e],[e],[e],[e]],
    [[c],[e],[e],[e],[e]],
    [[c],[c],[e],[e],[e]]
]).

/*
boardEmpty([
    [e,e,e,c,c],
    [e,e,e,e,c],
    [e,e,e,e,e],
    [c,e,e,e,e],
    [c,c,e,e,e]
]).
*/



writeColIndex(X) :-write('   | 1 | 2 | 3 | 4 | 5 |'), nl.
writeHeader(X) :- write('-----------------------+'), nl.
writeRowIndex(L) :- name(C, [L]),
                    format(' ~p |', [C]).

printCell([Top|_]) :- elem(Top, C),
                      format(' ~p |', [C]).

printLine([]) :- nl.
printLine([Head|Tail]) :-
    printCell(Head),
    printLine(Tail).

printBoard([]).
printBoard([Head|Tail], L) :-
    writeRowIndex(L),
    L1 is L+1, 
    printLine(Head),
    writeHeader(X),
    printBoard(Tail, L1).

printBoard :- 
    nl,
    writeColIndex(X),
    writeHeader(X),
    boardEmpty(X),
    printBoard(X, 65).



