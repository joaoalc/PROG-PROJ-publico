:- use_module(library(lists)).

elem(c, C) :- C = '_ '.
elem(wb, C) :- C = 'WB'.  % white Ball
elem(bb, C) :- C = 'BB'.  % black Ball
elem(wr, C) :- C = 'WR'.  % white ring
elem(br, C) :- C = 'BR'.   % black ring

initial([
    [[       ],[       ],[],[wb,wr,c],[wb,wr,c]],
    [[       ],[       ],[],[       ],[wb,wr,c]],
    [[       ],[       ],[],[       ],[       ]],
    [[bb,br,c],[       ],[],[       ],[       ]],
    [[bb,br,c],[bb,br,c],[],[       ],[       ]]
]).


intermediate([
    [[       ],[     ],[     ],[c         ],[c         ]],
    [[       ],[     ],[wb,wr],[wr        ],[bb,br,wr,c]],
    [[       ],[     ],[wb,wr],[br        ],[          ]],
    [[wb,wr,c],[bb,br],[bb,br],[wb,wr     ],[          ]],
    [[c      ],[c    ],[     ],[          ],[          ]]
]).

final([
    [[       ],[        ],[  ],[bb,br,c ],[bb,br,c   ]],
    [[       ],[        ],[  ],[br      ],[bb,br,wr,c]],
    [[       ],[        ],[wr],[        ],[          ]],
    [[wb,wr,c],[wb,wr,br],[  ],[        ],[          ]],
    [[wr,c   ],[wb,wr,c  ],[  ],[        ],[          ]]
]).

emptyBoard([
    [[e],[e],[e],[c],[c]],
    [[e],[e],[e],[e],[c]],
    [[e],[e],[e],[e],[e]],
    [[c],[e],[e],[e],[e]],
    [[c],[c],[e],[e],[e]]
]).



/*---DISPLAY FUNCTIONS---*/
writeColIndex(X) :-write('   |  1   |  2   |  3   |  4   |  5   |'), nl.
writeHeader(X) :- write('--------------------------------------+'), nl.
writeRowIndex(L) :- name(C, [L]),
                    format(' ~p |', [C]).
/*
% prints top element of cell stack
printCell([]) :- write('    |'). %base case empty cell
printCell([Top|_]) :- elem(Top, C), 
                      format(' ~p |', [C]).
                      */

/*Prints first 2 elements of a list*/
printElements([],2) :- write('      ').
printElements([],1) :-write('   ').
printElements([],0).
printElements([Top|Head], 0).
printElements([Top|Rest], N) :- elem(Top, C),
                                N1 is N-1,
                                format('~p ', [C]),
                                printElements(Rest, N1).

% prints board row
printLine([]) :- nl.
printLine([Head|Tail]) :-
    printElements(Head,2),
    write('|'),
    printLine(Tail).

% prints board matrix
printMatrix([],L).
printMatrix([Head|Tail], L) :-
    writeRowIndex(L),
    L1 is L+1,          
    printLine(Head),
    writeHeader(X),
    printMatrix(Tail, L1).

% input player name
readPlayer(Msg,X) :- 
    format('~n ~s ~n', [Msg]),
    read(X).

displayGame(GameState, Player) :-    
    format('~n Player: ~p ~n', [Player]),   
    nl,                         %start printing board
    writeColIndex(X),           %index from 1 to 5
    writeHeader(X),             %separator
    printMatrix(GameState,65).




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
