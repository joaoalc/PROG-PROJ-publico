getEnemyColor(white, black).
getEnemyColor(black, white).

getRingColor(wr, white).
getRingColor(br, black).

%max2(-Max, +Line, +Column)
%max2 gives the points given/taken by a ball depending on it's position.
max2(Max, 1, 0) :-
    Max is 0.

max2(Max, 1, 0) :-
    Max is 0.

max2(Num1, Num1, Num1).

max2(Max, Num1, Num2) :-
    (Num1 > Num2 ->
    Max is Num1;
    Num1 < Num2 ->
    Max is Num2).

%getMoveDistance(+Line, +Column, +Color, -Result) (Color is always black or white)
%getMoveDistance gives the points given/taken by a ball depending on it's position and color.
getMoveDistance(Line, Col, black, Result) :-
    ColOff is 4 - Col,
    max2(Result, Line, ColOff).

getMoveDistance(Line, Col, white, Result) :-
    LineOff is 4 - Line,
    max2(Result, LineOff, Col).



%calcPoints(+Piece, +Line, +Column, +Color, -Result) (Color is always black or white)
%calcPoints gives the points given/taken by a ball depending on it's position, color and the the player's color.
calcPoints(First, Line, Col, Color, Result) :-
    (isEnemyBall(Color, First) ->
        getEnemyColor(Color, EneColor),
        getMoveDistance(Line, Col, EneColor, MoveScore),
        Result is MoveScore*0.4;
    isOwnBall(Color, First) ->
        getMoveDistance(Line, Col, Color, MoveScore),
        Result is -MoveScore;
    Result is 0).

%calculatePointsSunkRings(+PieceList, +Color, RingColor, -Result) (Color and RingColor is always black or white)
%calculatePointsSunkRings gives the points given/taken by the "sunk" rings in each cell depending on their position on the board and color.
%PieceList starts on the second ring of each cell, because only covering rings should increase/decrease the result, not placing rings.
%Color is the color of the turn's player
%RingColor is the color of the first/top ring of the cell
calculatePointsSunkRings([], _, _, Result) :-
    Result is 0.

calculatePointsSunkRings([Head|Rest], Color, RingColor, Result) :-
    calculatePointsSunkRings(Rest, Color, RingColor, Res),
    (isRing(Head) ->
        (getRingColor(Head, RingColor) ->
            (getRingColor(Head, Color) ->
                Resu is -0.6; 
                Resu is 0.6 
            );

            (getRingColor(Head, Color) ->
                Resu is -0.5; 
                Resu is 0.5
            )
        );
        Resu is 0
    ),
    Result is Resu + Res.


%calculatePointsCell(+CellPieces, +Line, +Col, +PlayerColor, -Result) (Color is always black or white)
%Calculates the points of every piece inside the cell.
calculatePointsCell([], Line, Col, Color, Total) :-
    Total is 0.

calculatePointsCell([First|Rest], Line, Col, Color, Total) :-
    (isRing(First) ->
        getRingColor(First, RingColor),
        calculatePointsSunkRings(Rest, Color, RingColor, Resul),
        Total is Resul;
        calcPoints(First, Line, Col, Color, Result),
        calculatePointsCell(Rest, Line, Col, Color, Resu),
        Total is Resu + Result*3
    ).

%calculatePointLine(+LinePieces, +Line, +PlayerColor, -Result, +CurrentColumn) (Color is always black or white)
%Calculates the points of every piece inside the line.
calculatePointLine([], _, _, Total, _) :- Total is 0.

calculatePointLine([First|Rest], Line, Color, Total, N) :-
    N1 is N + 1,
    calculatePointLine(Rest, Line, Color, Totl, N1),
    calculatePointsCell(First, Line, N, Color, Result),
    Total is Totl + Result.

%calculatePointsBoard(+BoardPieces, +PlayerColor, -Result, +CurrentLine) (Color is always black or white)
%Calculates the points of each board.
calculatePointsBoard([], _, Total, _) :- Total is 0.

calculatePointsBoard([First|Rest], Color, Total, N) :-
    N1 is N + 1,
    calculatePointsBoard(Rest, Color, Totl, N1),
    calculatePointLine(First, N, Color, Result, 0),
    Total is Totl + Result.

%value(+Board, +Player, -Value)
%Gets the value of each board as described in the "Board Evaluation" segment of the READLME
value(Board, Player, Value) :-
    isEndGame(Board, _),
    Value is 200.

value(Board, Player, Value) :-
    getPlayerColor(Player, Color),
    calculatePointsBoard(Board, Color, Value, 0).

%calcValueBoards(+Boards, +PlayerId, -Scores)
%Gets the value of each board and fills the Scores list with them.
calcValueBoards([], Color, []).
calcValueBoards([CurBo|AllBoards], PlayerID, [CurSco|Scores]) :-
    value(CurBo, PlayerID, CurSco),
    calcValueBoards(AllBoards, PlayerID, Scores).

%getBestBoards(+AllBoards, +Scores, -SelectedBoard)
%Gets the board with the highest score (picks a random board if there are multiple).
getBestBoards(AllBoards, Scores, SelectedBoard) :-
    my_max(Scores, R),
    filterBestBoards(AllBoards, Scores, BestBoards, R),
    random_member(SelectedBoard, BestBoards).

%filterBestBoards(+Boards, +Scores, -BestBoards, +Max)
%Gets every board with the highest score, given a list of boards and a list with their respective scores.
filterBestBoards([], _, [], _).

filterBestBoards([CurBo|AllBoards], [CurSco|Scores], [CurBestBo|BestBoards], Max) :-
    CurSco =:= Max,
    filterBestBoards(AllBoards, Scores, BestBoards, Max),
    copy_term(CurBo, CurBestBo).
    
filterBestBoards([CurBo|AllBoards], [CurSco|Scores], BestBoards, Max) :-
    filterBestBoards(AllBoards, Scores, BestBoards, Max).