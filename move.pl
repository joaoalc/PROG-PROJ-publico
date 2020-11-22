executePlayerTurn(Board, Player, UpdatedBoard) :-
        getPlayerColor(Player, Color),
        executeMoves(Board, Color, UpdatedBoard).

executeMoves(Board, Color, UpdatedBoard) :-
        repeat,
                once(inputType(Color, Move)),   % input first move
                move(Board, Move, TmpBoard),
                getNth(0, Move, Type),
                isRingMove(Type) ->     % if first move was a ring move then the player can move one of his balls
                 (
                    repeat,
                        moveBall(TmpBoard, Move, Color, UpdatedBoard)
     
                 ).
                                
                             
                            
                        
% get coordinates of the previously placed ring                       
getRingDestCoords(RingMove, DestLine, DestCol) :-
        getNth(0, RingMove, Type), !,
        Type == 'R',
        getNth(2, RingMove, DestLine),
        getNth(3, RingMove, DestCol).
                            
getRingDestCoords(RingMove, DestLine, DestCol) :-
        getNth(0, RingMove, Type), !,
        Type == 'MR',
        getNth(4, RingMove, DestLine),
        getNth(5, RingMove, DestCol).

% choose and move a ball to the cell with the previously placed ring                               
moveBall(Board, RingMove, Color, NewBoard) :-
        (askForSecondMove ->
                ( % execute second move
                    (
                          once(inputBallMove(Color, BallMove)),       % get ball location
                          getNth(2, BallMove, BallY), 
                          getNth(3, BallMove, BallX),
                          getTopXY(Board, BallX, BallY, Top), !,
                          isBall(Top),                                % input location must be a ball
                          getRingDestCoords(RingMove, DestLine, DestCol),
                          append(BallMove, [DestLine, DestCol], NewMove), !,
                          move(Board, NewMove, NewBoard)
                    );
                       
                        (nl, write('[i] Could not execute secondMove'), nl,
                        fail)
                ); % move on
                % no move executed
                % assign previous board to new one
                append(_, Board, NewBoard)
        ).
        
                   
                    
                    
                                      
                
                     
                     
isRingMove('MR').
isRingMove('R').

validAnswer('yes', y).
validAnswer('no', n).
validAnswer(y, y).
validAnswer(n, n).

% false if no, true if yes
askForSecondMove :-
     repeat,
        once(inputString('Move a ball? (yes/no)', Answer)),     
        validAnswer(Answer, Value),         %validate input
        !,
        (Value == y);
        fail.
                                        
        
        
        
        
