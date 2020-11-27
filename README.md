# **MITSUDOMOE**
# **PLOG - TP1 3MIEIC4 MITSUDOMOE_4**

| Name             | Number    |
| ---------------- | --------- |
| João Cardoso     | 201806531 |
| Ivo Saavedra     | 201707093 |

## **Description**
The objective of this game is to lay stepping stones (rings) for the player's trio (balls) in order to 
to walk and vault their way onto the opposite corner of the board.

The game consists of one gameboard depicting a 5x5 grid, 6 balls (3 white and 3 black) and 16 rings (8 in white and the other 8 in black).

## **Installation and Execution**
There are no necessary steps for running this game. One only needs to have SICStus installed.

## **Rules**
+ Each player has access to 3 balls and 8 rings of the color they choose.
+ Once a component - ring or ball - is at play it may never leave the board.
+ A ball can only sit directly on a ring of the same color.
+ A component is only allowed to move to adjacent (orthogonally or diagonally adjacent) tiles.
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
The destination can be any space that does not have ball on top, and if that space already has a ring on it then the player must place his ring on top.

#### **2- Ball Placement**
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
The board consists of a list of lists. A row is represented by a list of lists as well, in which the interior lists (stacks) represent each cell of the board. In our game a cell can have more than one element in it and because of that we decided that the best way of representing a cell would be a stack. The first element of the cell is the top of the stack and the last is the bottom.

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
As a simplification our displayGame function only prints the top 2 elements of each cell. We found that this change makes the board more intelligible for the player, as it reduces the amount of unnecessary information on screen.

The displayGame function starts by printing the header with the current player's name, color and available rings, then calls the displayBoard predicate which prints the current game state.

#### **Initial State:**
Each player starts with 3 balls positioned in the board's corners.

![Initial Board State](/images/initialState.png "Initial Board State") 

#### **Intermediate State:**
In this state the both player have reached one of the goal spaces for their balls (WB in D1 and BB in B5).

![Intermediate Board State](/images/intermediateState.png "Intermediate Board State")

#### **Final State:**
In the final state the player with the black pieces reached the goal spaces with all of his 3 balls first, so he wins the game.

![Final Board State](/images/finalState.png "Final Board State") 

### **List of Valid Moves**

### **Move Execution**

### **Game Over**

### **Bot Moves**

## **Conclusions**



### Sources
https://nestorgames.com/#mitsudomoe_detail



