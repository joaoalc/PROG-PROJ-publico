:- use_module(library(lists)).

elem(e, C) :- C = '  '.
elem(c, C) :- C = ' _'.
elem(wb, C) :- C = 'WB'.  % white Ball
elem(bb, C) :- C = 'BB'.  % black Ball
elem(wr, C) :- C = 'WR'.  % white ring
elem(br, C) :- C = 'BR'.   % black ring

initialBoard([
    [[e   ],[e   ],[e],[bb,c],[bb,c]],
    [[e   ],[e   ],[e],[e   ],[bb,c]],
    [[e   ],[e   ],[e],[e   ],[e   ]],
    [[wb,c],[e   ],[e],[e   ],[e   ]],
    [[wb,c],[wb,c],[e],[e   ],[e   ]]
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
printBoard([],L).
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


%Print board layer N
%N is the "Layer" it is printing


printBoardLayer(N) :-
    nl,
    writeColIndex(X),
    writeHeader(X),
    initialBoard(X),
    printBoardLayer(N, X, 65).

printCellLayer(N, []) :- write('    |').

printCellLayer(N, [Top|_]) :- N is 1, elem(Top, C), 
                      format(' ~p |', [C]).

printCellLayer(0, [Top|Rest], 1) :- 
    ((Top == 'wb' ; Top == 'bb'), elem(Top, C), format(' ~p |', [C])) ;
     ((Top \== 'wb' , Top \== 'bb'), elem(e, C) , format(' ~p |', [C])).

printCellLayer(0, [Top|Rest], N) :-  ((Top == 'wb' ; Top == 'bb'), A1 is 2, printCellLayer(A1, Rest, N)); ((Top \== 'bb' ; Top \== 'wb'), A1 is 2, printCellLayer(A1, [Top|Rest], N)).

printCellLayer(A, [], N) :- write('   |').

printCellLayer(A, [Top|Rest], N) :- (A =\= 0), A > 1, A < N, A1 is A + 1, printCellLayer(A1, Rest, N).

%printCellLayer(N, [Top|Rest]) :- N >= 2, (Top == 'wb',  N1 is N - 1, printCellLayer(N1, Rest)); printCellLayer(N, Rest).
printCellLayer(A, [Top|_], N) :- A == N, elem(Top, C), 
                      format(' ~p |', [C]).


%A == N == 0
printCellLayer(1, [Top|Tail], 1) :- ((Top == 'wb' ; Top == 'bb'), elem(Top, C), format(' ~p |', [C])) ; ((Top \== 'wb' ; Top \== 'bb'), elem(e, C), format(' ~p |', [C])).

% Lista vazia
printCellLayer(A, [], N) :- elem(e, C), format(' ~p |', [C]).

%A == 1
printCellLayer(1, [Top|Tail], N) :- N \== 1, (((Top == 'wb' ; Top == 'bb'), elem(Top, C), printCellLayer(2, Tail, N)) ; ((Top \== 'wb' ; Top \== 'bb'), printCellLayer(3, Tail, N))).

%Caso genérico
printCellLayer(A, [Top|Tail], N) :- A > 1, A < N, A1 is A + 1, printCellLayer(A1, Tail, N).

%A == N (chegou ao elemento da lista correta)
printCellLayer(A, [Top|Tail], N) :- Top \== [] , (Top \== 'c', A =:= N, elem(Top, C), format(' ~p |', [C])) ; (Top == 'c', A =:= N, elem('e', C), format(' ~p |', [C])).


printLineLayer(N, []) :- nl.

printLineLayer(N, [Head|Tail]) :-
    printCellLayer(1, Head, N),
    printLineLayer(N, Tail).


printBoardLayer(N, [], L).

printBoardLayer(N, [Head|Tail], L) :-
    writeRowIndex(L),
    L1 is L+1,          
    printLineLayer(N, Head),
    writeHeader(X),
    printBoardLayer(N, Tail, L1).
