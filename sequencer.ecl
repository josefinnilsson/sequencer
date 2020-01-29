bpmDiff([]).
bpmDiff([_]).
bpmDiff([X,Y|T]):-
  abs(X-Y) =< 5,
  bpmDiff([Y|T]).

shuffle([],[]).
shuffle(L, [H|T]):-
  append(V,[H|U],L),
  append(V,U,W),
  shuffle(W,T).

sequence(L1, L2):-
  shuffle(L1, L2),
  bpmDiff(L2).