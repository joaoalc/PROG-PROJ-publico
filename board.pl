:- use_module(library(lists)).

%Facts that indicate that piece A can be placed on piece B. Rings can also be played on nothing, but 
playableOn(wr, br).
playableOn(wr, wr).
playableOn(br, wr).
playableOn(br, br).
playableOn(wb, wr).
playableOn(bb, br).

%-------TODO: Not sure if these can be used for the purposes I want right now
playableOn(wr, none).
playableOn(br, none).
%-------


elem(none, '0'). % DEBUG

elem(c, 'C ').
elem(wb, 'WB').  % white Ball
elem(bb, 'BB').  % black Ball
elem(wr, 'WR').  % white ring
elem(br, 'BR').  % black ring

/*TODO is there a better way to compare? */
equalTo(c, c).
equalTo(wb, wb).  % white Ball
equalTo(bb, bb).  % black Ball
equalTo(wr, wr).  % white ring
equalTo(br, br).  % black ring

initial([
    [[       ],[       ],[],[wb,wr,c],[wb,wr,c]],
    [[       ],[       ],[],[       ],[wb,wr,c]],
    [[       ],[       ],[],[       ],[       ]],
    [[bb,br,c],[       ],[],[       ],[       ]],
    [[bb,br,c],[bb,br,c],[],[       ],[       ]]
]).

initial2([
    [[       ],[      bb ],[bb],[wb,wr,c],[wb,wr,c]],
    [[       ],[       ],[],[       ],[wb,wr,c]],
    [[       ],[       ],[br],[       ],[       ]],
    [[bb,br,c],[  bb     ],[],[       ],[       ]],
    [[bb,br,c],[bb,br,c],[],[       ],[       ]]
]).


intermediate([
    [[br     ],[     ],[     ],[c         ],[c         ]],
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



/*DISPLAY FUNCTIONS ------------------------------------------------------*/
writeColIndex(X) :-write('   |  1   |  2   |  3   |  4   |  5   |'), nl.
writeHeader(X) :- write('--------------------------------------+'), nl.
writeRowIndex(L) :- name(C, [L]),
                    format(' ~p |', [C]).
/*

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

displayBoard(Board) :-    
    nl,                         %start printing board
    writeColIndex(X),           %index from 1 to 5
    writeHeader(X),             %separator
    printMatrix(Board,65).


/*GET TOP ELEMENT BY INDEX ---------------------------------*/
getTopElem([], Piece) :- Piece = none.
getTopElem([First|_], Piece) :- Piece = First.

getCell([First|_], 0, Piece) :-
    getTopElem(First, Piece).

getCell([First|Rest], X, Piece) :-
    X > -1,
    X1 is X-1,
    getCell(Rest, X1, Piece).

getTopXY([First|_], X, 0, Piece) :-
    getCell(First, X, Piece).

getTopXY([First|Rest], X, Y, Piece) :-
    Y > -1,
    Y1 is Y-1,
    getTopXY(Rest, X, Y1, Piece).
    
/*REMOVE TOP ELEMENT ----------------------------------------*/
% removes top element off a cell at XY and returns it in Piece
popTopXY(BoardIn, Y, X, BoardOut, Piece) :-
    selectLine(BoardIn, Y, Line),
    selectCell(Line, X, Cell),
    popTop(Cell, NewCell, Piece),
    replace(Line, X, NewCell, NewLine),
    replace(BoardIn, Y, NewLine, BoardOut).
