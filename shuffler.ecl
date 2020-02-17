:- local struct(track(id, artist, bpm, length)).
:- lib(ic).

getArtist(track(_,A,_,_), A).
getLength(track(_,_,_,L), L).
printList(List):-
  (
    foreach(E, List) do
    write(E), write(", ")
  ),
  writeln("---").

distance(Track, [H|_], 0):-
  A1 is getArtist(Track),
  A2 is getArtist(H),
  A1 == A2.
distance(_,[],99999999). %Check if MAXINT exist
distance(Track, [_|T], Distance):-
  distance(Track, T, Length),
  L is getLength(Track),
  Distance is L + Length.

shuffle([],[]).
shuffle(L, [H|T]) :-
  append(V,[H|U],L),
  append(V,U,W),
  shuffle(W,T).

getTail(_, [], []).
getTail(Elem, [H|T], T) :-
  Elem == H.
getTail(Elem, [_|T], Tail) :-
  getTail(Elem, T, Tail).

shuffler(Tracks, Perm) :-

  shuffle(Tracks, Perm),

  ( foreach(T, Perm), param(Perm) do
    getTail(T, Perm, Tail),
    distance(T, Tail, Dist),!, %Maybe doesn't compute correct value
    Dist $>= 9
  ).

%% search(Perm,3,input_order,indomain,complete,[]).
%% shuffler([track(1, 'ABBA', 185, 2),track(2, 'Moto Boy', 180, 3),track(3, 'Lana Del Rey', 170, 3),track(4, 'Kent', 175, 4),track(5, 'ABBA', 190, 3)],X).