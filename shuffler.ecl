:- local struct(track(id, artist, bpm, length)).
:- local struct(position(index, track)).
:- local struct(swap(first, second)).
:- lib(ic).
:- lib(ic_global).
:- lib(lists).
:- lib(repair).

%----------------------------------------------------------------------
% Helper functions
%----------------------------------------------------------------------

getArtist(track(_,A,_,_), A).

getLength(track(_,_,_,L), L).

getTail(_, [], []).

getTail(Elem, [H|T], T) :-
  Elem == H.
getTail(Elem, [_|T], Tail) :-
  getTail(Elem, T, Tail).

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

add(X,[],[X]).
add(X,[Y|Tail],[Y|Tail1]):-
  add(X,Tail,Tail1).

swapHelp([],_,_,_,_).

swapHelp([X|[]],Var1,Var2,Swapped, NewSwapped):-
  X == Var1,
  add(Var2, Swapped, NewSwapped),
  swapHelp([], Var1, Var2, Swapped, NewSwapped).

swapHelp([X|[]],Var1,Var2,Swapped, NewSwapped):-
  X == Var2,
  add(Var1, Swapped, NewSwapped),
  swapHelp([], Var1, Var2, Swapped, NewSwapped).

swapHelp([X|[]],Var1,Var2,Swapped, NewSwapped):-
  add(X, Swapped, NewSwapped),
  swapHelp([], Var1, Var2, Swapped, NewSwapped).

swapHelp([H|T], Var1, Var2, Swapped, Final):-
  H == Var2,
  add(Var1,Swapped, NewSwapped),
  swapHelp(T, Var1, Var2, NewSwapped, Final).

swapHelp([H|T], Var1, Var2, Swapped, Final):-
  H == Var1,
  add(Var2,Swapped, NewSwapped),
  swapHelp(T, Var1, Var2, NewSwapped, Final).

swapHelp([H|T],Var1,Var2,Swapped, Final):-
  add(H,Swapped, NewSwapped),
  swapHelp(T, Var1, Var2, NewSwapped, Final).

swap(List, Var1, Var2, X) :- swapHelp(List, Var1, Var2, [], X),!.

get_tentative_vals(List, Res) :-
  (foreach(R, Res), foreach(I, List) do
    I tent_get R
  ).

getPlayback(Indices, Tracks, Playback) :-
  (foreach(P, Playback), foreach(T, Tracks), foreach(I, Indices) do
    P = position{index: I, track: T}
  ).

tracksFromPlayback(Playback, Tracks) :-
  sort(1, <, Playback, Sorted),
  getTracks(Sorted, Tracks).

%----------------------------------------------------------------------
% Constraints
%----------------------------------------------------------------------
distance(Track, [H|_], 0):-
  A1 is getArtist(Track),
  A2 is getArtist(H),
  A1 == A2.

distance(_,[],99999999).

distance(Track, [_|T], Distance):-
  distance(Track, T, Length),
  L is getLength(Track),
  Distance is L + Length.

artistConstraint(Track, Tracks, ConstraintDistance, Opt) :-
   getTail(Track, Tracks, Tail),
   distance(Track, Tail, Distance),!,
   Distance >= ConstraintDistance r_conflict arcons,
   Opt tent_is Distance.

%----------------------------------------------------------------------
% Shuffler
%----------------------------------------------------------------------

shuffler(Tracks, PlaybackFinal):-
  length(Tracks, N),
  length(Playback, N),
  length(Indices, N),

  tent_init(Indices),

  getPlayback(Indices, Tracks, Playback),

  tracksFromPlayback(Playback, SortedTracks),
  defineConstraints(SortedTracks, OptSum),

  hill_climb(Indices, SortedTracks, BestOrder, 0, Final),

  get_tentative_vals(Final, Positions),
  getPlayback(Final, Tracks, PlaybackFinal).

defineConstraints(Tracks, OptSum) :-
  ( foreach(T, Tracks), foreach(O, Opts), param(Tracks) do
    artistConstraint(T, Tracks, 3, O)
  ),
  sumlist(Opts, OptsSum),
  OptSum tent_is OptsSum
  .

tent_init(List) :-
  ( foreach(Var,List),foreach(I, [0,1,2,3,4,5]) do %TODO: Dynamic length
    Var tent_set I
  ).

makeSwap(Indices, Tracks, SwappedFinal) :- % Här inne ska OptSum uppdateras
  get_tentative_vals(Indices, Res),
  select_constraint_var(Res, Val1),
  select_var(Res, Val1, Val2),
  swap(Res, Val1, Val2, SwappedFinal),

  (foreach(P, Playback), foreach(T, Tracks), foreach(I, SwappedFinal) do
    P = position{index: I, track:T} % Connect playback index with track
  ),

  tracksFromPlayback(Playback, SortedTracks),

  defineConstraints(SortedTracks, OptSum)
.

hill_climb([],_,Indices,_,Indices).
hill_climb(Indices, Tracks, BestOrder, Counter, FinalPlayback) :-
  (Counter >= 5 ->
    hill_climb([],_,Indices,_, FinalPlayback)
  ;
    (conflict_constraints(arcons, Conflict),
    length(Conflict, Old),
    makeSwap(Indices, Tracks, Swapped),
    conflict_constraints(arcons, NewConflict),
    length(NewConflict, NewVal),
    (NewVal < Old -> % This doesn't work as constraints get appended, arcons would have to be cleared
      hill_climb([],_,_,_,_,FinalPlayback)
    ;
      NewCounter is Counter + 1,
      hill_climb(Swapped, Tracks, Indices, NewCounter, FinalPlayback)
      )
    )
  ).

set_to_tent(Term) :-
  Term tent_get Tent,
  Term = Tent.

select_var(List, CVar, Var) :-
  member(Var, List),
  CVar \== Var.

select_constraint_var(List, Var) :-
  member(Var, List).

%----------------------------------------------------------------------
% Entry Point
%----------------------------------------------------------------------

run(Tracks, Perm) :-
  cputime(StartTime),

  shuffler(Tracks, Perm),!,

  TimeUsed is cputime-StartTime,
  printf("Goal took %.2f seconds%n", [TimeUsed]).
