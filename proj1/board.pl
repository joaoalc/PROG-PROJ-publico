:- use_module(library(lists)).

%Facts that indicate that piece A can be placed on piece B. Rings can also be played on nothing
playableOn(wr, br).
playableOn(wr, wr). 
playableOn(wr, c).
playableOn(br, c).  
playableOn(br, wr).
playableOn(br, br).
playableOn(wb, wr).
playableOn(bb, br).
playableOn(wr, none). % for empty cells
playableOn(br, none).



elem(none, '0'). % DEBUG

elem(c, 'C ').   % corner
elem(wb, 'WB').  % white Ball
elem(bb, 'BB').  % black Ball
elem(wr, 'WR').  % white ring
elem(br, 'BR').  % black ring

/* GAME STATES -------------------------------------------*/
initial([
    [[       ],[       ],[],[wb,wr,c],[wb,wr,c]],
    [[       ],[      ],[],[       ],[wb,wr,c]],
    [[       ],[       ],[],[       ],[       ]],
    [[bb,br,c],[       ],[],[       ],[       ]],
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


/*DISPLAY FUNCTIONS ------------------------------------------------------*/
writeColIndex :-write('   |  1   |  2   |  3   |  4   |  5   |'), nl.
writeHeader :- write('--------------------------------------+'), nl.
% writeRowIndex(+Letter)
writeRowIndex(L) :- name(C, [L]),
                    format(' ~p |', [C]).

% printElements(+Stack, -N)
% Prints first 2 elements of a list
printElements([],2) :- write('      ').
printElements([],1) :-write('   ').
printElements([],0).
printElements([Top|Head], 0).
printElements([Top|Rest], N) :- elem(Top, C),   % decode top element
                                N1 is N-1,
                                format('~p ', [C]),
                                printElements(Rest, N1). % print next

% printLine(+Line)
% prints board row
printLine([]) :- nl.
printLine([Head|Tail]) :-
    printElements(Head,2),
    write('|'),
    printLine(Tail).

% printMatrix(+Board)
% prints board matrix
printMatrix([],L).
printMatrix([Head|Tail], L) :-
    writeRowIndex(L),
    L1 is L+1,          
    printLine(Head),
    writeHeader,
    printMatrix(Tail, L1).

% display board
displayBoard(Board) :-    
    nl,                         %start printing board
    writeColIndex,           %index from 1 to 5
    writeHeader,             %separator
    printMatrix(Board,65).


/*GET TOP ELEMENT BY INDEX ---------------------------------*/
% getTopXY(+Board, +Col, +Line, -Piece)
% returns top piece at position (Col, Line)
getTopXY([First|_], X, 0, Piece) :-
    getCell(First, X, Piece).

getTopXY([First|Rest], X, Y, Piece) :-
    Y > -1,
    Y1 is Y-1,
    getTopXY(Rest, X, Y1, Piece).

getTopElem([], Piece) :- Piece = none. % returns none when cell is empty
getTopElem([First|_], Piece) :- Piece = First.

getCell([First|_], 0, Piece) :-
    getTopElem(First, Piece).

getCell([First|Rest], X, Piece) :-
    X > -1,
    X1 is X-1,
    getCell(Rest, X1, Piece).


    
/*REMOVE TOP ELEMENT ----------------------------------------*/
% popTopXY(+BoardIn, +Line, +Col, -BoardOut, -Piece)
% removes top element off a cell at XY and returns it in Piece
popTopXY(BoardIn, Y, X, BoardOut, Piece) :-
    selectLine(BoardIn, Y, Line),
    selectCell(Line, X, Cell),
    popTop(Cell, NewCell, Piece),
    replace(Line, X, NewCell, NewLine),
    replace(BoardIn, Y, NewLine, BoardOut).
