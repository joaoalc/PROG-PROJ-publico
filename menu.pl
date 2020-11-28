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

printElement([]).
printElement([Head|Rest]) :-
    printElementLine(Head),
    printElement(Rest).

printElementLine([Line|_]) :-
    nl,
    write(Line).

printSeparator :-
    write('-----------------------------------------------------------------').


printMenu :-
    write('\33\[2J'),   % clear Screen
    header(X),
    printSeparator,
    printElement(X),
    options(Y),
    printElement(Y),
    nl, nl, 
    printSeparator.