:- use_module(library(between)).
:- use_module(library(random)).

% decrementBotStash(BotID)
% Decrements the number of rings the current bot/AI has
decrementBotStash(BotID) :-
    player(BotID, CurrName, CurrColor, CurrStash, 1), % get current player
    retract(player(BotID, _, _, _, _)),
    NewSize is CurrStash-1,
    asserta(player(BotID, CurrName, CurrColor, NewSize, 1)).

% executeBallMove(+In, +TypeOfMove, -Out)
% moves balls for AI
executeBallMove(In, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], Out) :-
    once(isValidBallMove(In, [_,Color,SrcLine,SrcCol,DestLine,DestCol])),
    moveBallBot(In, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], Out).

% moveBallBot(+Board, +TypeOfMove, -UpdatedBoard)
% move is not a vault (move ball)
moveBallBot(Board,['MB', Color, SrcLine, SrcCol, DestLine, DestCol], UpdatedBoard) :-
        \+isVault(SrcLine, SrcCol, DestLine, DestCol),
        !, % if not a vault, list of moves is the move itself
        move(Board, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], UpdatedBoard).

% in case it is a vault
moveBallBot(Board, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], UpdatedBoard) :-
        fetchVaultedBalls(Board, Color, SrcLine, SrcCol, DestLine, DestCol, CoordsList),
        !,
        (length(CoordsList, 0) ->       % only call vault assistant is there are balls to displace
                (move(Board, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], UpdatedBoard));
                (move(Board, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], TmpBoard), % perform vault move
                executeVaultMovesBot(TmpBoard, CoordsList, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], UpdatedBoard)) % relocate opponent's balls
        ).
% checkStash
% Checks if the stash isn't empty yet
checkStash :-
    getPlayerTurn(ID, 1),
    getStashSize(ID, Size), !,
    Size > 0.

% executeVaultMovesBot(+Board, +SourceCoordinatesList, +TypeOfMove, -UpdatedBoard).
% if the vaulted balls list is empty then no vault is performed
executeVaultMovesBot(UpdatedList, List, _, UpdatedList) :- length(List,0).

% the initial ball move is only called at the end of all the intermidiate vaults
executeVaultMovesBot(Board, CoordsList, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], UpdatedBoard) :-
        once((copy_term(CoordsList, TmpList),
        getNth(0, CoordsList, First))),
        relocateBallsBot(Board, First, TmpBoard),
        deleteNth(0, CoordsList, NewList),
        executeVaultMovesBot(TmpBoard, NewList, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], UpdatedBoard).  

% relocates all opponent's vaulted balls
relocateBallsBot(Board, [SrcLine, SrcCol], UpdatedBoard) :-
    between(0,4, DestLine), between(0,4, DestCol),
    once(getTopXY(Board, SrcCol, SrcLine, Ball)),
    isValidBallRelocation(Board, [_,Ball,_,_,DestLine,DestCol]),
    executeMove('RB', Board, [_,_,SrcLine,SrcCol,DestLine,DestCol], UpdatedBoard).

%moveGenerator(+TypeOfMove, +Color, +In, -Out)
% generate second moves
moveGenerator('MB', Color, In, Out) :-
    between(0,4,SrcLine), between(0,4,SrcCol),
    between(0,4,DestLine), between(0,4,DestCol),
    once(executeBallMove(In, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], Out)).


% generate ring movements
moveGenerator('MR', Color, In, Out) :-
    between(0,4,SrcLine), between(0,4,SrcCol),
    between(0,4,DestLine), between(0,4,DestCol),
    once(move(In, ['MR', Color, SrcLine, SrcCol, DestLine, DestCol], Out)).
    
moveGenerator('R', Color, In, Out) :-
    checkStash,
    between(0,4,Line), between(0,4,Col),
    once(move(In, ['R', Color, Line, Col], Out)).

% generate moves starting with ring placements
moveGenerator('SM', Color, In, Out) :-
    between(0,4,Line), between(0,4,Col),
    once(move(In, ['R', Color, Line, Col], Tmp)),
    between(0,4,SrcLine), between(0,4,SrcCol),
    between(0,4,DestLine), between(0,4,DestCol),
    once(executeBallMove(Tmp, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], Out)).

% generate second moves starting with a ring movements
moveGenerator('SM2', Color, In, Out) :-
    between(0,4,SrcLineR), between(0,4,SrcColR),
    between(0,4,DestLineR), between(0,4,DestColR),
    once(move(In, ['MR', Color, SrcLineR, SrcColR, DestLineR, DestColR], Tmp)),
    between(0,4,SrcLine), between(0,4,SrcCol),
    between(0,4,DestLine), between(0,4,DestCol),
    once(executeBallMove(Tmp, ['MB', Color, SrcLine, SrcCol, DestLine, DestCol], Out)).

/* --------------------------------- VALID MOVES ------------------------- */
% valid_moves(+GameState, +Player, -ListOfMoves)
% get all valid moves of the Player bot and places them in ListOfMoves
valid_moves(GameState, Player, ListOfMoves) :-
    getPlayerColor(Player, Color),
    findall(NewGameState,   % TODO put in valid_moves predicate 
    (moveGenerator(Type, Color, GameState, NewGameState)), 
    ListOfMoves),
    nl,
    length(ListOfMoves, L),
    format('[i] Generated  ~p moves', L).
   
    

printAll(Board, 0).
printAll([Head|Rest], Ind) :-
    displayBoard(Head),
    I is Ind-1,
    printAll(Rest, I).


% chooseMove(+GameState, +Player, +BotDifficulty, -Move)
% best move for Lvl 1 bots (highest level)
chooseMove(GameState, Player, 1, Move) :-
    valid_moves(GameState, Player, ListOfMoves),
    calcValueBoards(ListOfMoves, Player, Scores),
    getBestBoards(ListOfMoves, Scores, Move),
    checkStashChange(Player, Move).

% random moves for Lvl 0 bots (lowest level)
chooseMove(GameState, Player, 0, Move) :-
    valid_moves(GameState, Player, ListOfMoves),
    random_member(Move, ListOfMoves),
    checkStashChange(Player, Move).   

% checkStashChange(+Player, +GameState)
% Checks if the bot placed a new ring with it's last play. Decrements the number of rings in the stash if so
checkStashChange(Player, GameState) :-
    checkStash,
    once(countRings(GameState, Player, N)),
    getStashSize(Player, Size),
    Size + N > 8,
    decrementBotStash(Player).

checkStashChange(_,_).
    
    

% countRings(+GameState, +Player, -NumberOfRingsInBoard)
% Gets the number of rings in a board of the Player's color. This is needed because each player can only have up to 8 balls in play.
countRings(GameState, Player, N) :-
    getPlayerColor(Player, Color),
    selectRing(Color, Ring),
    countRingsBoard(GameState, Ring, N).

% countRingsCell(+BoardPieces, +RingToCount, -NumberOfRingsInBoard)
% Gets the number of rings in a board of the Player's color.
countRingsBoard([], _, 0).
countRingsBoard([First|Rest], Ring, N) :-
    countLines(First, Ring, N1),
    countRingsBoard(Rest, Ring, N2),
    N is N1+N2.

% countRingsCell(+BoardLinePieces, +RingToCount, -NumberOfRingsInLine)
% Gets the number of rings in a board's line of the Player's color.
countLines([], _, 0).
countLines([First|Rest], Ring, N) :-
    countRingsCell(First, Ring, NC),
    countLines(Rest, Ring, NL),
    N is NC+NL.

% countRingsCell(+BoardCellPieces, +RingToCount, -NumberOfRingsInCell)
% Gets the number of rings in a board's cell of the Player's color.
countRingsCell([Top|Rest], Ring, N) :-
    Top == Ring,
    countRingsCell(Rest, Ring, N1),
    N is N1+1.
countRingsCell([_|Rest], Ring, N) :- countRingsCell(Rest, Ring, N).
