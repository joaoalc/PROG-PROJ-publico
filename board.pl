:- use_module(library(lists)).

elem(e, C) :- C = '  '.
elem(c, C) :- C = ' _'.
elem(wb, C) :- C = 'WB'.  % white Ball
elem(bb, C) :- C = 'BB'.  % black Ball
elem(wr, C) :- C = 'WR'.  % white ring
elem(br, C) :- C = 'BR'.   % black ring

initial([
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



/*---DISPLAY FUNCTIONS---*/
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
printMatrix([],L).
printMatrix([Head|Tail], L) :-
    writeRowIndex(L),
    L1 is L+1,          
    printLine(Head),
    writeHeader(X),
    printMatrix(Tail, L1).


displayGame(GameState, Player) :- 
    initial(GameState),         %attribute initial board to game state
    nl,                         %start printing board
    writeColIndex(X),           %index from 1 to 5
    writeHeader(X),             %separator
    printMatrix(GameState,65).



play :- 
    displayGame(State, Player).






/*DEBUG*/
%Print board layer N
%N is the "Layer" its printing

printBoardLayer(N) :-
    nl,
    writeColIndex(X),
    writeHeader(X),
    initialBoard(X),
    printBoardLayer(N, X, 65).


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
