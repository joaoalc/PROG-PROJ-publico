
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
          isLinearMove(SrcLine, SrcCol, DestLine, DestCol), !,   % checks if inputed movement is linear
          vaultVerification(Board, SrcLine, SrcCol, DestLine, DestCol),
          getTopXY(Board, DestCol, DestLine, Top), !,
          playableOn(Ball, Top). 
 
isValidBallMove(_, _, _)  :- nl, write('[i] Invalid ball move'), nl, fail.  

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

   
/*VAULT VERIFICATION*/
vaultVerification(Board, SrcLine, SrcCol, DestLine, DestCol) :-
    linearBallSearch(Board, SrcLine, SrcCol, DestLine, DestCol).

vaultVerification(_, _, _, _,_) :- nl, write('[i] Invalid vault'), nl, fail.
 
searchStep(X,Y, Step) :- Y-X >= 0,
                         Step is 1.
searchStep(_,_, Step) :- Step is -1.
                                            

% no need to vault check for moves to adjacent tiles
linearBallSearch(_, SrcLine, SrcCol, DestLine, DestCol) :-
    DifLine is SrcLine-DestLine,
    abs2(DifLine, Y),
    Y > 1,
    DifCol is SrcCol-DestCol,
    abs2(DifCol, X),
    X > 1.

% horizontal movements
linearBallSearch(Board, SrcLine, SrcCol, DestLine, DestCol) :-
    SrcLine =:= DestLine,
    searchStep(SrcLine, DestLine, Step),
    horizontalBallSearch(Board, SrcLine, SrcCol, DestCol, Step).

%vertical movements
linearBallSearch(Board, SrcLine, SrcCol, DestLine, DestCol) :-
    SrcCol =:= DestCol,
    searchStep(SrcCol, DestCol, Step),
    verticalBallSearch(Board, SrcLine, DestLine, SrcCol, Step).

/*horizontal vault verification*/
% reached target cell
horizontalBallSearch(_, _, SrcCol, DestCol, Step) :- 
    SrcCol+Step =:= DestCol.

% horizontal search
horizontalBallSearch(Board, Line, SrcCol, DestCol, Step) :-
    Col is SrcCol+Step,
    getTopXY(Board, Col, Line, Top), !,
    isBall(Top),
    horizontalBallSearch(Board, Line, Col, DestCol, Step).

/*vertical vault verification*/
% reached target cell
verticalBallSearch(_, SrcLine, DestLine, _, Step) :-
    SrcLine+Step =:= DestLine.

verticalBallSearch(Board, SrcLine, DestLine, Col, Step) :-
    Y is SrcLine+Step,
    selectLine(Board, Y, Line),
    selectCell(Line, Col, Cell),
    getTop(Cell, Top), !,
    isBall(Top),
    verticalBallSearch(Board, Y, DestLine, Col, Step).
    
                                 
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





