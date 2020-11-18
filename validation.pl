isValidMove(Move, MovesList) :-
    memberchk(Move, MovesList).


validMoves(Board, PlayerPiece, ValidPlays) :-
    getValidTopXY(Board, Line, Col, PlayerPiece, ValidPlays).

verifyTopElem([], Line, Col, PlayerPiece, Head) :- write('non'), append(_, ['R', PlayerPiece, Line, Col], Head).

verifyTopElem([Top|_], Line, Col, PlayerPiece, Head) :-
    write(Top),
    nl,
    playableOn(PlayerPiece, Top) ->
        append(_, ['R', PlayerPiece, Line, Col], Head);
        fail.

/*
getValidSpot([First|Rest], Line, Col, PlayerPiece, [Head|ValidPlays]) :-
    Line < 5,
    L1 is Line + 1,
    (verifyTopElem(First, Line, Col, PlayerPiece, NewPlays) ->
    append(_, NewPlays, Head),
    getValidSpot(Rest, L1, Col, PlayerPiece, ValidPlays);
    write('Fail'),
    getValidSpot(Rest, L1, Col, PlayerPiece, [Head|ValidPlays])
    ).*/
tmp :- getValidSpot([[br     ],[br       ],[],[br,wr,c],[br,wr,c]], 0, 0, wr, [Head| List]), write(List).



app(X, [Head|ValidPlays]) :- append(_, X, Head).

getValidSpot(_, 5, _, _, _).


getValidSpot([First|Rest], 4, Col, PlayerPiece, ValidPlays) :-
    verifyTopElem(First, 4, Col, PlayerPiece, NewPlays) ->
    app(NewPlays, ValidPlays);
    true.



getValidSpot([First|Rest], Line, Col, PlayerPiece, [Head|ValidPlays]) :-
    Line < 4,
    L1 is Line + 1,
    (verifyTopElem(First, Line, Col, PlayerPiece, NewPlays) ->
    append(_, NewPlays, Head),
    getValidSpot(Rest, L1, Col, PlayerPiece, ValidPlays);
    write('Fail'),
    getValidSpot(Rest, L1, Col, PlayerPiece, [Head| ValidPlays])
    ),
    write('List right now: '),
    write([Head|ValidPlays]),
    nl,
    write('Its head'),
    write(Head),
    nl,
    write('The rest'),
    write(ValidPlays),
    nl.

getValidTopXY([First|Rest], Line, Col, PlayerPiece, ValidPlays, ResultList) :-
    Col < 5,
    /*write(First),
    nl,*/
    nl,
    getValidSpot(First, Line, Col, PlayerPiece, ValidPlays),
    Col1 is Col+1,
    getValidTopXY(Rest, Line, Col1, PlayerPiece, ValidPlays).

getValidTopXY(_, _, 5, _, _).

isValidMove(Board, Move) :-
    getNth(0, Move, Type),
    getNth(1, Move, Piece),
    getNth(2, Move, Line),
    getNth(3, Move, Col),
    Type == 'R' ->
        isValidRingMove(Board, Piece, Line, Col);
    true.




/*
getTopElem([], Piece) :- Piece = none.
getTopElem([First|_], Piece) :- Piece = First.

getCell([First|_], 0, Piece) :-
    getTopElem(First, Piece).

getCell([First|Rest], X, Piece) :-
    X1 is X-1,
    getCell(Rest, X1, Piece).

getTopXY([First|_], X, 0, Piece) :-
    getCell(First, X, Piece).

getTopXY([First|Rest], X, Y, Piece) :-
    Y1 is Y-1,
    getTopXY(Rest, X, Y1, Piece).
*/

/*
validMoves(Board, PlayerID, ValidPlays) :-
    getValidRingMoves(Board, PlayerID, ValidPlays),
    write(ValidPlays).*/

    %TODO calculate valid movement plays

/*RING PLACEMENT VALIDATION-----------------------*/
getValidRingMoves(Board, PlayerID, Ret) :-
    getStashSize(PlayerID, Size),
    Size > 0,
    getPlayerColor(PlayerID, Color),        % get current player s color
    selectPiece('R', Color, Piece),        % select ring with the color of the player
    calcLine(Board, Piece, 0, Ret).



isValidRingMove(Board, Piece, Line, Col) :-
    getTopXY(Board, Line, Col, Top),
    !,
    playableOn(Piece, Top). % assert if ring is playable on top of stack X Y


calcLine(Board, Piece, Line, List) :-
    write(Line),
    Line < 5,
    calcCol(Board, Piece, Line, 0, New),
    write('List of accepted elements on line '), write(Line), write('is '), write(New), nl,
    append(_, New, List),
    L1 is Line+1,
    calcLine(Board, Piece, L1, List).

calcLine(_,_,_,Ret).
/*
calcCol(Board, Piece, Line, 4, [Head]) :- 
    evaluateCell(Board, Piece, Line, 4, Res) ->
        append(_, Res, Head).*/
calcCol(Board, Piece, Line, Col, [Head|Tail]) :-
    Col < 5,
    write(Line),
    write(Col),
    nl,
    (evaluateCell(Board, Piece, Line, Col, Res) ->
        append(_, Res, Head);
    true),
    C1 is Col+1,
    calcCol(Board, Piece, Line, C1, Tail).

calcCol(_, _, _, _, Ret).

evaluateCell(Board, Piece, Line, Col, Ret) :-
    isValidRingMove(Board, Piece, Line, Col),
    append(_, ['R', Piece, Line, Col], Ret).

% evaluateCell(_,_,_,_,[]).





