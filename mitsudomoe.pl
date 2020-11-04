:-consult('board.pl').

play :- 
    readPlayer('Player #1: ',Player1),
    readPlayer('Player #2: ',Player2),
    intermediate(State),         %attribute initial board to game state
    displayGame(State, Player1).
