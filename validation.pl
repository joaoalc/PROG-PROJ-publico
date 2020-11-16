
isValidMove(Move, MovesList) :-
    memberchk(Move, MovesList).

validMoves(Board, PlayerID, ValidPlays) :-
    getValidRingMoves(Board, PlayerID, ValidPlays).
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
    Line < 5,
    calcCol(Board, Piece, Line, 0, New),
    append(_, New, List),
    L1 is Line+1,
    calcLine(Board, Piece, L1, List).

calcLine(_,_,_,Ret).

calcCol(Board, Piece, Line, 4, [Head]) :- 
    evaluateCell(Board, Piece, Line, 4, Res) ->
        append(_, Res, Head).
calcCol(Board, Piece, Line, Col, [Head|Tail]) :-
    Col < 5,
    evaluateCell(Board, Piece, Line, Col, Res) ->
        append(_, Res, Head),
    C1 is Col+1,
    calcCol(Board, Piece, Line, C1, Tail).

calcCol(_, _, _, _, Ret).

evaluateCell(Board, Piece, Line, Col, Ret) :-
    isValidRingMove(Board, Piece, Line, Col),
    append(_, ['R', Piece, Line, Col], Ret).

% evaluateCell(_,_,_,_,[]).





