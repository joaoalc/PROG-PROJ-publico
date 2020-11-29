
%validMoves(Board, PlayerPiece, ValidPlays)

isBall(wb).
isBall(bb).
isRing(wr).
isRing(br).

% isValidMove(+Board, +Move)
% true if a given move is valid
isValidMove(Board, Move) :-
    getNth(0, Move, Type), !,
    (Type == 'R' ->                                      % place ring from stash
            isValidRingPlacement(Board, Move);
    Type == 'MB' ->                                      % move top piece from a given cell
            isValidBallMove(Board, Move);
    Type == 'MR' ->                                      % move ring
            isValidRingMove(Board, Move);
            nl, write('[X] Invalid Type'), nl, fail
    ).

% information on the goal cells for each player
goalCell(white, wb, 3, 0).  % D1
goalCell(white, wb, 4, 0).  % E1
goalCell(white, wb, 4, 1).  % E2

goalCell(black, bb, 0, 3).   % A4
goalCell(black, bb, 0, 4).   % A5
goalCell(black, bb, 1, 4).   % B5

% enemyIsAtGoal(+Color, +Piece, +Line, +Col)
% checks if an enemie's ball is at it's goal cell (can't move)
enemyIsAtGoal(white, Top, Line,Col) :- goalCell(black, Top, Line, Col).
enemyIsAtGoal(black, Top, Line,Col) :- goalCell(white, Top, Line, Col).

% checks if a ball is at it's goal cell (can't move)
ballIsAtGoal(Color, Top, Line, Col) :- goalCell(Color, Top, Line,Col).

% isValidBallMove(+GameState, +Move)
% true when a given ball move is valid
isValidBallMove(Board, [_,Color,SrcLine,SrcCol,DestLine,DestCol]) :-
          getTopXY(Board, SrcCol, SrcLine, Ball), !,             % get Piece at position X Y
          isBall(Ball), !, selectBall(Color, Ball), !,           % piece verifications  (ball of the same color of the player)  
          \+ballIsAtGoal(Color, Ball, SrcLine, SrcCol), !,       % can't move balls that have reached their goal                           
          isLinearMove(SrcLine, SrcCol, DestLine, DestCol), !,   % checks if inputed movement is linear
          vaultVerification(Board, Color, SrcLine, SrcCol, DestLine, DestCol),  % check for vaults
          getTopXY(Board, DestCol, DestLine, Top), !,            % get top piece is destination cell
          playableOn(Ball, Top).                                 % verify if ball is playable on top piece

% isValidBallRelocation(+GameState, +Move)
% check if a ball can be relocated after a vaulting operation
isValidBallRelocation(Board, [_,Ball,_,_,DestLine,DestCol]) :-
    once(getTopXY(Board, DestCol, DestLine, Top)),
    playableOn(Ball, Top).

% isValidRingPlacement(+GameState, +Move)
% true when a given ring placement is valid
isValidRingPlacement(Board, [_,Color,Line,Col]) :-
    selectRing(Color, Ring),
    getTopXY(Board, Col, Line, Top),    % get top element from destination cell
    !,
    playableOn(Ring, Top). % assert if ring is playable on top of stack X Y

% true when a ring movement is valid
isValidRingMove(Board, [_,Color,SrcLine,SrcCol,DestLine,DestCol]) :-
    getTopXY(Board, SrcCol, SrcLine, Ring),
    isRing(Ring), !, selectRing(Color, Ring),        % verify if selected piece is a player's piece
    getTopXY(Board, DestCol, DestLine, Top),           % get top piece on destination cell
    !,
    playableOn(Ring, Top).                            % assert if ring is playable on top of stack X Y

% isLinearMove(+SourceLine, +Sourcecol, +DestLine, +DestCol)
 % checks if a ball is being played linearly 
isLinearMove(SrcLine, SrcCol, DestLine, DestCol) :-
    (SrcLine =:= DestLine);     % horizontal
    (SrcCol =:= DestCol);       % vertical
    (                           % diagonal
        M1 is SrcLine-DestLine,
        M2 is SrcCol-DestCol,
        M is M1/M2,
        abs2(M, 1.0)
     ).

   
/*VAULT VERIFICATION -----------------------------------------------------*/
% vaultVerification(+GameState, +Color, +SrcLine, +SrcCol, +DestLine, +DestCol)
% checking if move is not a vault
vaultVerification(_, _, SrcLine, SrcCol, DestLine, DestCol) :-
    \+isVault(SrcLine, SrcCol, DestLine, DestCol).

% in case it is a vault, the linear verifications are performed
vaultVerification(Board, Color, SrcLine, SrcCol, DestLine, DestCol) :-
    linearVault(Board, Color, SrcLine, SrcCol, DestLine, DestCol).

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


% linearVault(+GameState, +Color, +SrcLine, +SrcCol, +DestLine, +DestCol)
% determines vault orientation (horizontal, vertical, diagonal) and calls the respective vaulting verification function
% horizontal movements
linearVault(Board, Color, SrcLine, SrcCol, DestLine, DestCol) :-
    SrcLine =:= DestLine,
    searchStep(SrcCol, DestCol, Step),      % search direction
    selectLine(Board, SrcLine, Line), !,
    horizontalVault(Line, SrcLine, Color, SrcCol, DestCol, Step).

%vertical movements
linearVault(Board, Color, SrcLine, SrcCol, DestLine, DestCol) :-
    SrcCol =:= DestCol,
    searchStep(SrcLine, DestLine, Step), !, % search direction
    verticalVault(Board, Color, SrcLine, DestLine, SrcCol, Step).

% diagonal movements
linearVault(Board, Color, SrcLine, SrcCol, DestLine, DestCol) :-
    searchStep(SrcCol, DestCol ,StepX),
    searchStep(SrcLine, DestLine, StepY), !, % search direction
    diagonalVault(Board, Color, SrcLine, SrcCol, DestLine, DestCol, StepY, StepX).

% horizontalVault(+Line, +LineIndex, +SrcCol, +DestCol, +Step)
% horizontal vault verification
% reached target cell
horizontalVault(_,_,_, SrcCol, DestCol, Step) :- 
    SrcCol+Step =:= DestCol.

% horizontal search
horizontalVault(Line, LineIndex, Color, SrcCol, DestCol, Step) :-
    Col is SrcCol+Step,
    selectCell(Line, Col, Cell),
    getTop(Cell, Top), !,                              % get top element in cell
    isBall(Top), !,
    \+enemyIsAtGoal(Color, Top, LineIndex, Col),       % can't relocate balls that have reached their goal
    horizontalVault(Line, LineIndex, Color, Col, DestCol, Step).

% verticalVault(+GameState, +Color, +SrcLine, +DestLine, +Col, +Step)
% vertical vault verification
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

% diagonalVault(+GameState, +Color, +SrcLine, +SrcCol, +DestLine, +DestCol, +StepY, +StepX)  
%diagonal vault verification
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
                                    



