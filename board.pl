:- use_module(library(lists)).

elem(e, C) :- C = '  '.
elem(c, C) :- C = ' _'.
elem(wb, C) :- C = 'WB'.  % white Ball
elem(bb, C) :- C = 'BB'.  % black Ball
elem(wr, C) :- C = 'WR'.  % white ring
elem(br, C) :- C = 'BR'.   % black ring

initialBoard([
    [[e],[e],[e],[bb,c],[bb,c]],
    [[e],[e],[e],[e],[bb,c]],
    [[e],[e],[e],[e],[e]],
    [[wb,c],[e],[e],[e],[e]],
    [[wb,c],[wb,c],[e],[e],[e]]
]).


emptyBoard([
    [[e],[e],[e],[c],[c]],
    [[e],[e],[e],[e],[c]],
    [[e],[e],[e],[e],[e]],
    [[c],[e],[e],[e],[e]],
    [[c],[c],[e],[e],[e]]
]).




writeColIndex(X) :-write('   |  1 |  2 |  3 |  4 |  5 |'), nl.
writeHeader(X) :- write('----------------------------+'), nl.
writeRowIndex(L) :- name(C, [L]),
                    format(' ~p |', [C]).

% prints top element of cell stack
printCell([Top|_]) :- elem(Top, C), 
                      format(' ~p |', [C]).

% prints board row
printLine([]) :- nl.
printLine([Head|Tail]) :-
    printCell(Head),
    printLine(Tail).

% prints board matrix
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
    initialBoard(X),
    printBoard(X, 65).



