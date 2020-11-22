
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
    Type == 'MB' ->                                      % move top piece from a given cell
            isValidBallMove(Board, Color, Move);
    Type == 'MR' ->
            isValidRingMove(Board, Color, Move);
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
    (SrcLine =:= DestLine);     % horizontal
    (SrcCol =:= DestCol);       %vertical
    (                           %diagonal
        M1 is SrcLine-DestLine,
        M2 is SrcCol-DestCol,
        M is M1/M2,
        abs2(M, 1.0)
     ).

isLinearMove(_,_,_,_) :-
    nl, write('[i] Balls can only move linearly'), nl, fail.

   
/*VAULT VERIFICATION -----------------------------------------------------*/
% checking if move is not a vault
vaultVerification(_, SrcLine, SrcCol, DestLine, DestCol) :-
    \+isVault(SrcLine, SrcCol, DestLine, DestCol).

% in case it is a vault, the linear verifications are performed
vaultVerification(Board, SrcLine, SrcCol, DestLine, DestCol) :-
    linearVault(Board, SrcLine, SrcCol, DestLine, DestCol), write(vault).

vaultVerification(_, _, _, _,_) :- nl, write('[i] Invalid vault'), nl, fail.

% calculate step direction 
searchStep(X,Y, Step) :- Y-X >= 0,
                         Step is 1.
searchStep(_,_, Step) :- Step is -1.
  

% no need to vault check for moves to adjacent tiles 
% true when the number of travelled cells in each direction is greatter then 1
isVault(SrcLine, SrcCol, DestLine, DestCol) :-
    (
            DifLine is SrcLine-DestLine,
            abs2(DifLine, Y),
            Y > 1
    );
    (
        DifCol is SrcCol-DestCol,
         abs2(DifCol, X),
        X > 1
    ).
isVault(_,_,_,_) :- nl, write('not a vault'), fail.                                     


% horizontal movements
linearVault(Board, SrcLine, SrcCol, DestLine, DestCol) :-
    SrcLine =:= DestLine,
    searchStep(SrcCol, DestCol, Step),
    selectLine(Board, SrcLine, Line),
    horizontalVault(Line, SrcCol, DestCol, Step).

%vertical movements
linearVault(Board, SrcLine, SrcCol, DestLine, DestCol) :-
    SrcCol =:= DestCol,
    searchStep(SrcLine, DestLine, Step),
    verticalVault(Board, SrcLine, DestLine, SrcCol, Step).

linearVault(Board, SrcLine, SrcCol, DestLine, DestCol) :-
    searchStep(SrcCol, DestCol ,StepX),
    searchStep(SrcLine, DestLine, StepY),
    diagonalVault(Board, SrcLine, SrcCol, DestLine, DestCol, StepY, StepX).

/*horizontal vault verification*/
% reached target cell
horizontalVault(_, SrcCol, DestCol, Step) :- 
    SrcCol+Step =:= DestCol.

% horizontal search
horizontalVault(Line, SrcCol, DestCol, Step) :-
    Col is SrcCol+Step,
    selectCell(Line, Col, Cell),
    getTop(Cell, Top), !,
    isBall(Top),
    horizontalVault(Line, Col, DestCol, Step).

/*vertical vault verification*/
% reached target cell
verticalVault(_, SrcLine, DestLine, _, Step) :-
    SrcLine+Step =:= DestLine.
% vertical search
verticalVault(Board, SrcLine, DestLine, Col, Step) :-
    Y is SrcLine+Step,
    selectLine(Board, Y, Line),
    selectCell(Line, Col, Cell),
    getTop(Cell, Top), !,
    isBall(Top),
    verticalVault(Board, Y, DestLine, Col, Step).
    
/*diagonal vault verification*/
%reached target cell
diagonalVault(_, SrcLine, _, DestLine, _, StepY, _) :-
    SrcLine+StepY =:= DestLine.
%diagonal search
diagonalVault(Board, SrcLine, SrcCol, DestLine, DestCol, StepY, StepX) :-
    X is SrcCol+StepX,
    Y is SrcLine+StepY,
    selectLine(Board, Y, Line),
    selectCell(Line, X, Cell),
    getTop(Cell, Top), !,
    isBall(Top),
    diagonalVault(Board, Y, X, DestLine, DestCol, StepY, StepX).
                                    



