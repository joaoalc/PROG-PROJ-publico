# **MITSUDOMOE**
# **PLOG - TP1 3MIEIC4 MITSUDOMOE_4**

| Name             | Number    |
| ---------------- | --------- |
| João Cardoso     | 201806531 |
| Ivo Saavedra     | 201707093 |

## **Description**
The objective of this game is for each player to traverse the board with all his three balls by placing rings on it, with the end goal of reaching the opposite corner (goal corner) of the board with all of his balls.

The game consists of one gameboard depicting a 5x5 grid, 6 balls (3 white and 3 black) and 16 rings (8 in white and the other 8 in black).

## **Installation and Execution**
There are no necessary steps for running this game. One only needs to have SICStus installed.

## **Rules**
+ Each player has access to 3 balls and 8 rings of the color they choose.
+ Once an element - ring or ball - is at play it may never leave the board.
+ A ball can only sit directly on a ring of the same color.
+ An element is only allowed to move to adjacent (orthogonally or diagonally adjacent) tiles.
    - If an adjacent tile has another ball the player can vault over it.
    - The player must relocate every opponent's balls wich he has vaulted over.
+ Once a ball reaches one of it's goal spaces, it never leaves that space.

### **How to play**
In each turn the player should:
    1. Place or move one of his rings;
    2. Move one of his balls;
    3. Place or move a ring and then move a ball.
If the player is unable to perform any of these steps, then the game ends and the opponent wins.

#### **1- Ring Placement**
The player can place a new ring from the supply, or move one of his exposed rings (the ring cannot have a ball or another ring on top).
The destination can be any cell that does not have a ball on top, and if that space already has a ring on it then the player must place his ring on top. After a ring movement the player can also move one of his balls if he desires.

#### **2- Ball Movement**
At his turn the player can move one of his balls that has not reached the goal spaces, either by moving it to an adjacent ring or by vaulting over 
other adjacent balls.
When the player vaults over another player's ball, then he must relocate it to a space of his choosing (as long as it has a ring with the same color as the ball being relocated). If there is no valid vault then the player cannot perform the movement which leads to the vault.

#### **End Game**
The game ends when one player gets all 3 balls on the opposite corner of the board or when one of the players does not have any valid moves.

## **Game Logic**
### **Elements**
Every board piece is represented by an atom. In order to print every element to the console we call the predicates listed bellow.
    
    elem(c, 'C').    % board corner (goal space)
    elem(wb, 'WB').  % white Ball
    elem(bb, 'BB').  % black Ball
    elem(wr, 'WR').  % white ring
    elem(br, 'BR').  % black ring

### **Board**
The board consists of a list of lists. A row is represented by a list of lists as well, in which the interior lists (stacks) represent each cell of the board. In our game, a cell can have more than one element in it and, because of that, we decided that the best way of representing a cell would be a stack. The first element of the cell is the top of the stack and the last is the bottom.

    /*Initial board*/
    initial([
        [[       ],[       ],[],[wb,wr,c],[wb,wr,c]],
        [[       ],[       ],[],[       ],[wb,wr,c]],
        [[       ],[       ],[],[       ],[       ]],
        [[bb,br,c],[       ],[],[       ],[       ]],
        [[bb,br,c],[bb,br,c],[],[       ],[       ]]
    ]).

    /*Intermediate board*/
    intermediate([
        [[       ],[     ],[     ],[c         ],[c         ]],
        [[       ],[     ],[wb,wr],[wr        ],[bb,br,wr,c]],
        [[       ],[     ],[wb,wr],[br        ],[          ]],
        [[wb,wr,c],[bb,br],[bb,br],[wb,wr     ],[          ]],
        [[c      ],[c    ],[     ],[          ],[          ]]
    ]).

    /*Final board*/
    final([
        [[       ],[        ],[  ],[bb,br,c ],[bb,br,c   ]],
        [[       ],[        ],[  ],[br      ],[bb,br,wr,c]],
        [[       ],[        ],[wr],[        ],[          ]],
        [[wb,wr,c],[wb,wr,br],[  ],[        ],[          ]],
        [[wr,c   ],[wb,wrc  ],[  ],[        ],[          ]]
    ]).


### **Console Visualization**
Our board consists 5x5 matrix in which the columns are numbered from 1 to 5 and rows are named from A to E.
As a simplification, our displayGame function only prints the top 2 elements of each cell. We found that this change makes the board more intelligible for the player, as it reduces the amount of unnecessary information on screen.

The displayGame function starts by printing the header with the current player's name, color and available rings, then calls the displayBoard predicate which prints the current game state.

#### **Menu**
In our menu we have the options for the three proposed gamemodes (player vs player, player vs computer and computer vs computer). The user must insert one of the 4 available options.

![Menu](/images/menu.png "Game Menu") 

#### **Initial State:**
Each player starts with 3 balls positioned in the board's corners.

![Initial Board State](/images/initialState.png "Initial Board State") 

#### **Intermediate State:**
In this state both players have reached at least one of the goal spaces for their balls (WB in D1, BB in B5 and BB in A5).

![Intermediate Board State](/images/intermediateState.png "Intermediate Board State")

#### **Final State:**
In the final state the player with the white pieces reached the goal spaces with all of his 3 balls first, so he wins the game.

![Final Board State](/images/finalState.png "Final Board State") 

#### **Move Input**
In every turn the *Type* prompt appears and the player must choose on of three inputs:
 - R - place a ring from stash
 - MR - move a ring on the board
 - MB - move a ball on the board

In this case the player chose to place one ring from his stash on the cell B4

![Move Input](/images/typeInput.png "Move Input") 

#### **Vaulting Assistant**
Every time a player vaults over the enemie's balls, then he must relocate them. To facilitate this we created a vaulting assistant were the positions of the vaulted balls are listed and the player can choose the order of the relocations.

In this case the player has vaulted over an enemie's ball located on B4.

![Vault Assistant](/images/vaultAssistant.png "Vault Assistant") 

### **List of Valid Moves**
In each turn of our game the player can perform one of three movement types as described in the *Rules* section. As a simplification we decided to calculate all the game states that are originated after a move has been performed. In order to obtain all the valid moves for a given player at a specific turn we used the *findall(+Template, :Goal, -Bag)* predicate in which the *+Template* is the new game state after the move has been performed, the *:Goal* is a generator predicate that receives the current game state, and returns the updated game state, and finally the *-Bag* is the list of game states to be returned by the *valid_moves* predicate.

### **Move Execution**
In order to execute and validate a player's move we implemented the *move(+GameState,+Move,-NewGameState)* predicate in which the *+GameState* is the current game state, *+Move* is the movement chosen by the player and *-NewGameState* is the updated game states after the move has been perfomed. Before executing the move we first validate it with the *isValidMove(+GameState, +Move)* predicate which in turn calls the respective validation predicate according to the received move type. The ring move types ('RM', 'R') are valid when the chosen destination cell doesn't have a ball on top. The ball movement type ('MB') is valid when the chosen destination cell has a ring of the same color of the ball on top.
After a successful validation the *executeMove(+Type, +GameState, +Move, -NewGameState)* predicate is called according to the move type *+Type*, the move is executed in the current game state *+GameState* and then updated state is return in *-NewGameState*.

### **Game Over**
The end game verification is performed by the *gameOver(+GameState, -Winner)* predicate which verifies the goal corners of the current gmae state and checks if one of the players has reached all of his goals. If one of the players has won, then *-Winner* will be the winner's id. If none of the players has finished then *-Winner* will be returned with a value of 0. 

### **Bot Moves**
We have 2 pickable difficulties for our bots to use.
The lowest level selects a random move from the list of valid moves.
The highest level uses the *value(+GameState, +Player, -Value)* predicate to obtain the values of each move. Then it randomly selects one of the moves that have the highest value.

## **Board Evaluation**
The *value(+GameState, +Player, -Value)* predicate determines the value of a valid move. The higher the result, the better the board is for that bot.
The result returned from the predicate does not depend on the last move, it just depends on the resulting board.
We iterate through the lines, columns and pieces of each board a single time and determine each board's score by adding up the scores of each piece in that circumstance.
The result is calculated as follows:
	-Each ball of the current bot's color reduces the result by 3.
	-Each ball of the enemy bot's color increases the result by 1.2. The significantly lower value of this relative to the value decremented is meant to avoid infinite loops.
	-Placing a ring on the same cell there is a ring of the same color reduces the result by 0.5 for each ring of that color in that cell previously.
	-Placing a ring on the same cell there is a ring of the other player's color increases the result by 0.5 for each ring of that color in that cell previously.
The main functions used for board evaluation are found in evaluateBoard.pl

## **Conclusions**
This project helped us improve our prolog programing skills as well as our problem solving capabilities. Until this semester, we have had almost no exposure to declaritive programming languages, so this assignement was definitely the best way of interiorizing the subjects discussed in the theoretical classes.
At the end of this project, having implemented a game with 3 game modes (player vs player, player vs computer, computer vs computer), and having created a reasonable ai we conclude that all the proposed objectives have been accomplished.


### Sources
https://nestorgames.com/#mitsudomoe_detail



