calculateValidPlays(Board, PlayerID, ValidPlays) :-
    getValidRingMoves(Board, PlayerID, ValidPlays).
   

getValidRingMoves(Board, PlayerID, Ret) :- 
    getStashSize(PlayerID, Size),
    Size > 0,
    getPlayerColor(PlayerID, Color),        % get current player's color
    selectPiece('R', Color, Piece),        % select ring with the color of the player
    findall(('R', Piece, Line, Col), 
        (
            isValidRingMove(Board, Piece, Line, Col),
            Line < 5,
            Col < 5
        ),
        Ret).

% getValidRingMoves(_,_,_). % no more valid ring moves


isValidRingMove(Board, Piece, Line, Col) :- 
    getTopXY(Board, Line, Col, Top),
    playableOn(Piece, Top). % assert if ring is playable on top of stack X Y

