
%validMoves(Board, PlayerPiece, ValidPlays)

isBall(wb).
isBall(bb).
isRing(wr).
isRing(br).

isValidMove(Board, Move) :-
    getNth(0, Move, Type),
    getNth(1, Move, Color), !,
    (Type == 'R' ->                                     % ring placement
            isValidRingPlacement(Board, Move);
    Type == 'MB' ->                                      % move top piece from a given cell
            isValidBallMove(Board, Color, Move);
    Type == 'MR' ->
            isValidRingMove(Board, Color, Move);
            nl, write('[X] Invalid Type'), nl, fail
    ).

goalCell(white, wb, 3, 0).  % D1
goalCell(white, wb, 4, 0).  % E1
goalCell(white, wb, 4, 1).  % E2

goalCell(black, bb, 0, 3).   % A4
goalCell(black, bb, 0, 4).   % A5
goalCell(black, bb, 1, 4).   % B5

% checks if a ball is at it's goal cell (can't move)
enemyIsAtGoal(white, Top, Line,Col) :- goalCell(black, Top, Line, Col).
enemyIsAtGoal(black, Top, Line,Col) :- goalCell(white, Top, Line, Col).

ballIsAtGoal(Color, Top, Line, Col) :- goalCell(Color, Top, Line,Col).
    
isValidBallMove(Board, Color, [_,_,SrcLine,SrcCol,DestLine,DestCol]) :-

          getTopXY(Board, SrcCol, SrcLine, Ball), !,   % get Piece at position X Y
          isBall(Ball), !, selectBall(Color, Ball), !,      % piece verifications  (ball of the same color of the player)  
          \+ballIsAtGoal(Color, Ball, SrcLine, SrcCol), !,       % can't move balls that have reached their goal                           
          isLinearMove(SrcLine, SrcCol, DestLine, DestCol), !,   % checks if inputed movement is linear
          vaultVerification(Board, Color, SrcLine, SrcCol, DestLine, DestCol),
          getTopXY(Board, DestCol, DestLine, Top), !,
          playableOn(Ball, Top). 
 
isValidBallMove(_, _, _)  :- %nl, write('[X] Invalid ball move'), nl, 
fail.  

% check if a ball can be relocated after a vaulting operation
isValidBallRelocation(Board, [_,Ball,_,_,DestLine,DestCol]) :-
    once(getTopXY(Board, DestCol, DestLine, Top)),
    playableOn(Ball, Top).

% isValidBallRelocation(_, _) :- %nl, write('[X] Invalid Relocation'), nl, fail.

isValidRingPlacement(Board, [_,Color,Line,Col]) :-
    selectRing(Color, Ring),
    getTopXY(Board, Col, Line, Top),
    !,
    playableOn(Ring, Top). % assert if ring is playable on top of stack X Y

isValidRingMove(Board, Color, [_,_,SrcLine,SrcCol,DestLine,DestCol]) :-
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
    %nl, write('[X] Balls can only move linearly'), nl,
     fail.

   
/*VAULT VERIFICATION -----------------------------------------------------*/
% checking if move is not a vault
vaultVerification(_, _, SrcLine, SrcCol, DestLine, DestCol) :-
    \+isVault(SrcLine, SrcCol, DestLine, DestCol).

% in case it is a vault, the linear verifications are performed
vaultVerification(Board, Color, SrcLine, SrcCol, DestLine, DestCol) :-
    linearVault(Board, Color, SrcLine, SrcCol, DestLine, DestCol).

vaultVerification(_, _, _, _, _,_) :- %nl, write('[X] Invalid vault'), nl,
    fail.

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


% horizontal movements
linearVault(Board, Color, SrcLine, SrcCol, DestLine, DestCol) :-
    SrcLine =:= DestLine,
    searchStep(SrcCol, DestCol, Step),
    selectLine(Board, SrcLine, Line), !,
    horizontalVault(Line, SrcLine, Color, SrcCol, DestCol, Step).

%vertical movements
linearVault(Board, Color, SrcLine, SrcCol, DestLine, DestCol) :-
    SrcCol =:= DestCol,
    searchStep(SrcLine, DestLine, Step), !,
    verticalVault(Board, Color, SrcLine, DestLine, SrcCol, Step).

linearVault(Board, Color, SrcLine, SrcCol, DestLine, DestCol) :-
    searchStep(SrcCol, DestCol ,StepX),
    searchStep(SrcLine, DestLine, StepY), !,
    diagonalVault(Board, Color, SrcLine, SrcCol, DestLine, DestCol, StepY, StepX).

/*horizontal vault verification*/
% reached target cell
horizontalVault(_,_,_, SrcCol, DestCol, Step) :- 
    SrcCol+Step =:= DestCol.

% horizontal search
horizontalVault(Line, LineIndex, Color, SrcCol, DestCol, Step) :-
    Col is SrcCol+Step,
    selectCell(Line, Col, Cell),
    getTop(Cell, Top), !,
    isBall(Top), !,
    \+enemyIsAtGoal(Color, Top, LineIndex, Col),       %can't relocate balls that have reached their goal
    horizontalVault(Line, LineIndex, Color, Col, DestCol, Step).

/*vertical vault verification*/
% reached target cell
verticalVault(_, _, SrcLine, DestLine, _, Step) :-
    SrcLine+Step =:= DestLine.
% vertical search
verticalVault(Board, Color, SrcLine, DestLine, Col, Step) :-
    Y is SrcLine+Step,
    selectLine(Board, Y, Line),
    selectCell(Line, Col, Cell),
    getTop(Cell, Top), !,
    isBall(Top), !,
    \+enemyIsAtGoal(Color, Top, Y, Col),
    verticalVault(Board, Color, Y, DestLine, Col, Step).
    
/*diagonal vault verification*/
%reached target cell
diagonalVault(_, _, SrcLine, _, DestLine, _, StepY, _) :-
    SrcLine+StepY =:= DestLine.
%diagonal search
diagonalVault(Board, Color, SrcLine, SrcCol, DestLine, DestCol, StepY, StepX) :-
    X is SrcCol+StepX,
    Y is SrcLine+StepY,
    selectLine(Board, Y, Line),
    selectCell(Line, X, Cell),
    getTop(Cell, Top), !,
    isBall(Top), !,
    \+enemyIsAtGoal(Color, Top, Y, X),
    diagonalVault(Board, Color, Y, X, DestLine, DestCol, StepY, StepX).
                                    



