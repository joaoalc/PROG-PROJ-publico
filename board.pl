:- use_module(library(lists)).

symbol(e, C) :- C = ' '.
symbol(c, C) :- C = '.'.
symbol(wb, C) :- C = 'WB'.  % white Ball
symbol(bb, C) :- C = 'BB'.  % black Ball
symbol(wr, C) :- C = 'WR'.  % white ring
symbol(br, C) :- C = 'BR'.   %black ring

boardEmpty([
    [e,e,e,corner,corner],
    [e,e,e,e,corner],
    [e,e,e,e,e],
    [corner,e,e,e,e],
    [corner,corner,e,e,e]
]).

boardPieces([
    [e,e,e,corner,corner],
    [e,e,e,e,corner],
    [e,e,e,e,e],
    [corner,e,e,e,e],
    [corner,corner,e,e,e]
]).


writeLine([]) :- write('|'), nl.
writeHeader(X) :- write('+-------------------+'), nl.
writeLine([Head|Tail]) :-
    symbol(Head, C),
    format('| ~p ', C),
    writeLine(Tail).

printBoard([]).
printBoard([Head|Tail]) :-
    writeLine(Head),
    writeHeader(X),
    printBoard(Tail).

printBoard :- 
    writeHeader(X),
    boardEmpty(X),
    printBoard(X),



