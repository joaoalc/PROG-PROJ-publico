# MITSUDOMOE
# PLOG - TP1 3MIEIC4 MITSUDOMOE_4

| Name             | Number    |
| ---------------- | --------- |
| João Cardoso     | 201806531 |
| Ivo Saavedra     | 201707093 |

## Description
The objective of this game is to lay stepping stones (rings) for the player's trio (balls) in order to 
to walk and vault their way onto the oposite board corner.

The game consists of one gameboard depicting a 5x5 grid, 6 balls (3 white and 3 black) and 16 rings (8 in white and the other 8 in black).

### Rules
+ Each player has access to 3 balls and 8 rings of the color they choose.
+ Once a component - ring or ball - is at play it may never leave the board.
+ A ball can only sit directly on a ring of the same color.
+ A component is only allowed to move to adjacent (orthogonally or diagonally adjacent) tiles.
    - If an ajacent tile has another component the player can vault over it.
+ Once a ball reaches one of it's goal spaces, it never leaves that space.

### How to play
In each turn the player should:
    1. Place or move one of his rings;
    2. Move one of his balls.
If the player is unhable to perform one or more of these steps, then he is out of the game.

#### 1- Ring Placement
The player can place a new ring from the supply, or move one of his exposed rings (the ring cannot have a ball or another ring on top).
The destination can be any space that does not have ball on top, and if that space already has a ring on it then the player must place is ring on top.

#### 2- Ball Placement
At his turn the player can move one of his balls that has not reached the goal spaces, either by moving it to an adjacent ring or by vaulting over 
other adjacent balls.
When the player vaults over another player's ball, then he must relocate it to a space of his choosing (as long as it has a ring with the same color as the ball being relocated). If there is no valid vault then the ball remains where it is.

#### End Game
The game ends when one the player gets all 3 balls on the opposite corner of the board.

### Game Representation
#### Elements
Every board pieces are represented by an atom. In order to print every element to the ocnsole we call the predicates listed bellow.
    
    elem(e, C) :- C = '  '.   % empty cell
    elem(c, C) :- C = ' _'.   % board corner (goal space)
    elem(wb, C) :- C = 'WB'.  % white Ball
    elem(bb, C) :- C = 'BB'.  % black Ball
    elem(wr, C) :- C = 'WR'.  % white ring
    elem(br, C) :- C = 'BR'.  % black ring

#### Board
The board consists of a list of lists. A row is represented by a list of lists aswell, in wich the interior lists (stacks) represent each cell of the board. In our game a cell can have more than one element in it and because of that we decided that the best of representing a cell would be a stack. The first element of the cell is the top of the stack and the last is the bottom.

    initialBoard([
    [[e   ],[e   ],[e],[bb,c],[bb,c]],
    [[e   ],[e   ],[e],[e   ],[bb,c]],
    [[e   ],[e   ],[e],[e   ],[e   ]],
    [[wb,c],[e   ],[e],[e   ],[e   ]],
    [[wb,c],[wb,c],[e],[e   ],[e   ]]
    ]).

    





