
%validMoves(Board, PlayerPiece, ValidPlays)

isBall(wb).
isBall(bb).
isRing(wr).
isRing(br).

isValidMove(Board, Move) :-
    getNth(0, Move, Type),
    getNth(1, Move, Color),
    (Type == 'R' ->                                     % ring placement
            isValidRingPlacement(Board, Color, Move);
    Type == 'M' ->                                      % move top piece from a given cell
            (isValidBallMove(Board, Color, Move);
            isValidRingMove(Board, Color, Move));
            nl, write('[i] Invalid Type'), nl, fail
    ).


isValidBallMove(Board, Color, Move) :-
          getNth(2, Move, SrcLine),
          getNth(3, Move, SrcCol),
          getTopXY(Board, SrcCol, SrcLine, Ball), !,   % get Piece at position X Y
          isBall(Ball), !, selectBall(Color, Ball),       % piece verifications  (ball of the same color of the player)                             
          getNth(4, Move, DestLine),
          getNth(5, Move, DestCol), !,  
          isLinearMove(SrcLine, SrcCol, DestLine, DestCol),   % checks if inputed movement is linear
          getTopXY(Board, DestCol, DestLine, Top), !,
          playableOn(Ball, Top).     

isValidRingPlacement(Board, Color, Move) :-
    selectRing(Color, Ring),
    getNth(2, Move, Line),
    getNth(3, Move, Col),
    getTopXY(Board, Col, Line, Top),
    !,
    playableOn(Ring, Top). % assert if ring is playable on top of stack X Y

isValidRingMove(Board, Color, Move) :-
    getNth(2, Move, SrcLine),
    getNth(3, Move, SrcCol),
    getTopXY(Board, SrcCol, SrcLine, Ring),
    isRing(Ring), !, selectRing(Color, Ring),
    getNth(4, Move, DestLine),
    getNth(5, Move, DestCol),                                   % verify if selected piece is a player's piece
    getTopXY(Board, DestCol, DestLine, Top),           % get top piece on destination cell
    !,
    playableOn(Ring, Top).                            % assert if ring is playable on top of stack X Y


/*checks if a ball is being played linearly*/
isLinearMove(SrcLine, SrcCol, DestLine, DestCol) :-
    (SrcLine =:= DestLine);
    (SrcCol =:= DestCol);
    (SrcLine-DestLine =:= SrcCol-DestCol). 

isLinearMove(_,_,_,_) :-
    nl, write('[i] Balls can only move linearly'), nl, fail.


   
    
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





