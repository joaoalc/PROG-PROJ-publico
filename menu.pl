header([
    [' __  __  ___  _____  ___  _   _  ___    ___   __  __   ___   ___'],
    ['|  \\/  ||_ _||_   _|/ __|| | | ||   \\  / _ \\ |  \\/  | / _ \\ | __|'],
    ['| |\\/| | | |   | |  \\__ \\| |_| || |) || (_) || |\\/| || (_) || _|' ],
    ['|_|  |_||___|  |_|  |___/ \\___/ |___/  \\___/ |_|  |_| \\___/ |___|']
]).

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

printMenu([]).
printMenu([Head|Rest]) :-
    printLine(Head),
    printMenu(Rest).

printLine([Line|_]) :-
    nl,
    write(Line).

printSeparator :-
    write('-----------------------------------------------------------------').


printTitle :-
    write('\33\[2J'),   % clear Screen
    header(X),
    printSeparator,
    printMenu(X),
    options(Y),
    printMenu(Y),
    nl, nl, 
    printSeparator.