:- local struct(track(id, artist, bpm, length)).

printList(List):-
  (
    foreach(E, List) do
    write(E), write(", ")
  ),
  writeln("---").

distance(_,[],999999).
distance(track{artist:A1}, [track{artist:A2}|_], 0):-
  A1 == A2.

distance(Track, [track{length:L}|T], Distance):-
  distance(Track, T, Length),
  Distance is L + Length.

shuffle2([],[]).
shuffle2(L, [H|T]) :-
  append(V,[H|U],L),
  append(V,U,W),
  shuffle(W,T).

getTail(_, [], []).
getTail(Elem, [H|T], T) :-
  Elem == H.
getTail(Elem, [_|T], Tail) :-
  getTail(Elem, T, Tail).

shuffler(Tracks, Perm) :-

  shuffle2(Tracks, Perm),

  ( foreach(T, Perm), param(Perm, T) do
    getTail(T, Perm, Tail),
    distance(T, Tail, Dist),
    eval(Dist) #>= 4
  ).
%% shuffler([track(1, 'ABBA', 185, 2),track(2, 'Moto Boy', 180, 3),track(3, 'Lana Del Rey', 170, 3),track(4, 'Kent', 175, 4),track(5, 'ABBA', 190, 3)]).