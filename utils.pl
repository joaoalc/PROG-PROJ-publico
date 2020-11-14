getNth(0, [Head|_], Head).
getNth(N, [_|Tail], Ret) :-
    N1 is N-1,
    getNth(N1, Tail, Ret).
