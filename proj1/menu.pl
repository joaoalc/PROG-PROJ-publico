% title element
header([
    [' __  __  ___  _____  ___  _   _  ___    ___   __  __   ___   ___'],
    ['|  \\/  ||_ _||_   _|/ __|| | | ||   \\  / _ \\ |  \\/  | / _ \\ | __|'],
    ['| |\\/| | | |   | |  \\__ \\| |_| || |) || (_) || |\\/| || (_) || _|' ],
    ['|_|  |_||___|  |_|  |___/ \\___/ |___/  \\___/ |_|  |_| \\___/ |___|']
]).

% options list element
options([
    [' '],
    ['+----------------------------+ '],
    ['| Please choose a game mode: |'],
    ['| 0 - Multiplayer            |'],
    ['| 1 - Singleplayer           |'],
    ['| 2 - Computer vs Computer   |'],
    ['| 3 - Quit                   |'],
    ['+----------------------------+']
]).

% printElement(+MenuElement)
% prints Menu list element
printElement([]).
printElement([Head|Rest]) :-
    printElementLine(Head),
    printElement(Rest).

printElementLine([Line|_]) :-
    nl,
    write(Line).

printSeparator :-
    write('-----------------------------------------------------------------').

% print entire menu
printMenu :-
    write('\33\[2J'),   % clear Screen
    header(X),
    printSeparator,
    printElement(X),
    options(Y),
    printElement(Y),
    nl, nl, 
    printSeparator.

% valid options
option(0).
option(1).
option(2).
option(3).

% select gamemodes: Player vs Player (Result = 0), Player vs Bot (Result = 1) or Bot vs Bot (Result = 2)
selectOption(Result) :-
    repeat,
        once(inputString('Option: ' , Result)),
        option(Result).

% select level
selectLevel(Lvl) :-
    write(' Level 0 - Random Moves'), nl,
    write(' Level 1 - Calculated Moves'), nl,
    repeat,
        once(inputString('Game Level: ', Lvl)),
        number(Lvl),
        Lvl >= 0, Lvl =< 1.