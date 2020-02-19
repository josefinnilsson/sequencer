:- local struct(track(id, artist, bpm, length)).
:- local struct(position(index, track)).
:- lib(ic).
:- lib(ic_global).

:- local variable(backtracks), variable(deep_fail).
init_backtracks :-
        setval(backtracks,0).
get_backtracks(B) :-
        getval(backtracks,B).
count_backtracks :-
        setval(deep_fail,false).
count_backtracks :-
        getval(deep_fail,false),
        setval(deep_fail,true),
        incval(backtracks),
        fail.

getArtist(track(_,A,_,_), A).
getLength(track(_,_,_,L), L).

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

artistDistance(Track, Tracks, Distance) :-
  getTail(Track, Tracks, Tail),
  distance(Track, Tail, Dist),!, %Maybe doesn't compute correct value
  Dist #>= Distance.

getIndex(position(I,_), I).

getIndices(List, Indices) :-
  ( foreach(L, List), foreach(I, Indices) do
    getIndex(L, I)
  ).

getTrack(position(_,T), T).

getTracks(List, Tracks) :-
  ( foreach(L, List), foreach(T, Tracks) do
    getTrack(L, T)
  ).


shuffler(Tracks, Perm) :-
  length(Tracks, N),
  length(Perm, N),
  length(Indices, N),

  (foreach(I, Indices), param(N) do
    I #:: 0..N
  ),

  ic_global:alldifferent(Indices),

  (foreach(P, Perm), foreach(T, Tracks), foreach(I, Indices) do
    indomain(I),
    P = position{index: I, track:T}
  ),

  sort(1, <, Perm, Sorted),
  getTracks(Sorted, SortedTracks),

  ( foreach(T, SortedTracks), param(SortedTracks) do
    count_backtracks,
    artistDistance(T, SortedTracks, 5)
  ),

  term_variables(Perm ,Vars),
  search(Vars, 1, first_fail, indomain, bbs(20), []),!.

run(Tracks, Perm) :-
  cputime(StartTime),
  init_backtracks,

  shuffler(Tracks, Perm),

  TimeUsed is cputime-StartTime,
  printf("Goal took %.2f seconds%n", [TimeUsed]),

  get_backtracks(B),
  printf("Solution found after %d backtracks%n", [B]).

%% run([track(1, 'ABBA', 185, 2),track(2, 'Moto Boy', 180, 3),track(3, 'Lana Del Rey', 170, 3),track(4, 'Kent', 175, 4),track(5, 'ABBA', 190, 3)],X).