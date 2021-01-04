
restrictSpot(FullNumberList, FullResultList, NumCols, NumLines, N) :-
    N =< NumCols * NumLines,
    nth1(N, FullNumberList, Number),
    Side1 is N - NumCols - 1,
    (Side1 > 0 ->
        (Side1 mod NumCols =:= 0 -> %Was N at the start of a line?
            Elem1 #= 0;
            nth1(Side1, FullResultList, Elem1)
        );
        Elem1 #= 0
    ),

    Side2 is N - NumCols,
    (Side2 > 0 ->
        nth1(Side2, FullResultList, Elem2);
        Elem2 #= 0
    ),
    
    
    Side3 is N - NumCols + 1,
    (Side3 > 0 ->
        (Side3 mod NumCols =:= 1 -> %Was N at the end of a line?
            Elem3 #= 0;
            nth1(Side3, FullResultList, Elem3)
        );
        Elem3 #= 0
    ),

    Side4 is N - 1,
    (Side4 mod NumCols =:= 0 -> %Was N at the start of a line?
        Elem4 #= 0;
        nth1(Side4, FullResultList, Elem4)
    ),

    nth1(N, FullResultList, ElemSelf),

    Side6 is N + 1,
    (Side6 mod NumCols =:= 1 ->
        Elem6 #= 0;
        nth1(Side6, FullResultList, Elem6)
    ),

    Side7 is N + NumCols - 1,
    (Side7 =< NumCols * NumLines ->
        (Side7 mod NumCols =:= 0 -> %Was N at the start of a line?
            Elem7 #= 0;
            nth1(Side7, FullResultList, Elem7)
        );
        Elem7 #= 0
    ),

    Side8 is N + NumCols,
    (Side8 =< NumCols * NumLines ->
        nth1(Side8, FullResultList, Elem8);
        Elem8 #= 0
    ),
    Side9 is N + NumCols + 1,
    (Side9 =< NumCols * NumLines ->
        (Side9 mod NumCols =:= 1 -> %Was N at the end of a line?
            Elem9 #= 0;
            nth1(Side9, FullResultList, Elem9)
        );
        Elem9 #= 0
    ),

    sum([Elem1, Elem2, Elem3, Elem4, Elem6, Elem7, Elem8, Elem9], #\=, Number),
    (
        ( % a light that has the same number of adjacent bulbs as it's  number can be on or off
            sum([Elem1, Elem2, Elem3, Elem4, 1, Elem6, Elem7, Elem8, Elem9], #=, Number), 
            ElemSelf #=1 %As long as the outermost condition is met, any element can be zero, but if this innermost contition is met, it can also be one
        );
        ElemSelf #= 0
    ),
    N1 is N + 1,
    restrictSpot(FullNumberList, FullResultList, NumCols, NumLines, N1).

restrictSpot(_, _, NumCols, NumLines, N) :-
    N > NumCols * NumLines.

