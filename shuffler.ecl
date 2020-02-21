:- local struct(track(id, artist, bpm, length)).
:- local struct(position(index, track)).
:- lib(ic).
:- lib(ic_global).
:- lib(lists).
:- lib(repair).

%----------------------------------------------------------------------
% Counting backtracks
%----------------------------------------------------------------------

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

swapHelp([],_,_,Swapped,X).

swapHelp([X|[]],Var1,Var2,Swapped, NewSwapped):-
  X == Var1,
  add(Var2, Swapped, NewSwapped),
  swapHelp([], Var1, Var2, Swapped, NewSwapped).

swapHelp([X|[]],Var1,Var2,Swapped, NewSwapped):-
  X == Var2,
  add(Var1, Swapped, NewSwapped),
  swapHelp([], Var1, Var2, Swapped, NewSwapped).

swapHelp([X|[]],_,_,Swapped, NewSwapped):-
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
  add(H,Swapped, NewSwapped),!,
  swapHelp(T, Var1, Var2, NewSwapped, Final).

swap(List, Var1, Var2, X) :- swapHelp(List, Var1, Var2, [], X),!.

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

artistDistance(Track, Tracks, Distance, B) :- %Should be true directly when Dist >= Distance
  getTail(Track, Tracks, Tail),
  distance(Track, Tail, Dist),!,
  Dist #>= Distance r_conflict cs,
  B tent_is Dist.

%----------------------------------------------------------------------
% Cost calculator
%----------------------------------------------------------------------
cost(Variable, Sequence, B) :-
  artistDistance(Variable, Sequence, 8, B).

totalCost(Sequence, BSum) :-
  BSum tent_is 0,
  ( foreach(Variable, Sequence), param(Sequence) do
    cost(Variable, Sequence, B),
    BSum tent_get Current,
    BSum tent_is Current + B
  ).

%----------------------------------------------------------------------
% Sequencer
%----------------------------------------------------------------------

shuffler(Tracks, Perm) :-
  length(Tracks, N),
  length(Perm, N),
  length(Indices, N),

  (foreach(I, Indices), param(N) do
    I #:: 0..N % Choose random index in playback sequence
  ),

  ic_global:alldifferent(Indices),

  (foreach(P, Perm), foreach(T, Tracks), foreach(I, Indices) do
    indomain(I),
    P = position{index: I, track:T} % Connect playback index with track
  ),

  sort(1, <, Perm, Sorted), % Get playback order
  getTracks(Sorted, SortedTracks),

  totalCost(SortedTracks, BSum),
  ( foreach(X, [1,2,3]), param(SortedTracks, BSum, Indices) do
    conflict_constraints(cs, List),
    select_constraint_var(List, Var1),
    select_var(List, Var1, Var2),
    swap(Indices, Var1, Var2, Swapped),

    % Get Tracks according to new playback order
    % calculate total cost for new playback
    % If cost is lower, set playback order to the new one
  ).

set_to_tent(Term) :-
  Term tent_get Tent,
  Term = Tent.

select_var(List, CVar, Var) :-
  member(Var, List),
  CVar \== Var.

select_constraint_var(List, Var) :-
  member(Constraint, List),
  term_variables(Constraint, Vars),
  member(Var, Vars).



%----------------------------------------------------------------------
% Entry Point
%----------------------------------------------------------------------

run(Tracks, Perm) :-
  cputime(StartTime),
  init_backtracks,

  shuffler(Tracks, Perm),!,

  TimeUsed is cputime-StartTime,
  printf("Goal took %.2f seconds%n", [TimeUsed]),

  get_backtracks(B),
  printf("Solution found after %d backtracks%n", [B]).