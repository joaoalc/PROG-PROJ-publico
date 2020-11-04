:- prolog_flag(single_var_warnings,_,off).
:-consult('board.pl').

play :- 
    readPlayer('Player #1: ',Player1),
    readPlayer('Player #2: ',Player2),
    initial(State),         %attribute initial board to game state
    displayGame(State, Player1),
    doIt(State).
/*Testing*/
doIt(Board) :-
    playPiece(Board, 1,1,wr,Res),
    displayGame(Res, 'TESTING ').

/*PIECE PLACEMENT*/
playPiece(BoardIn, Line, Col, Piece, BoardOut) :-                        %TODO line indexes are letters
    playLine(Lindex, BoardIn, Col, Piece, BoardOut).

 %TabOut=tabuleiro com  a peça
playLine(1, [Line | Rest], Col, Piece, [NewLine | Rest]) :- 
    playCol(Col, Line, Piece, NewLine). 

playLine(Lindex, [Line | Rest], Col , Piece, [Line | NewLines]) :- 
    M is Lindex-1,
    playLine(M, Rest, Col, Piece, NewLines).

%M is N -1

pushFront(Piece, List, [Piece|List]).

playCol(1, [Stack | MoreStacks], Piece, [Res|MoreStacks]) :-
    pushFront(Piece, Stack, Res).

playCol(N, [P | MoreCols], Piece, [P | NewPieces]) :-
    M is N-1, N > 1,
    playCol(M, MoreCols, Piece, NewPieces).
