:- lib(repair).
:- lib(ic).
:- lib(ic_global).

:- local struct(track(id, artist, duration)).
:- local struct(position(index, track)).
:- local struct(artist_distance(position1, position2, distance)).

%----------------------------------------------------------------------
% Get Functions
%----------------------------------------------------------------------

get_index(position(I,_),I).
get_track(position(_,T),T).

get_duration(track(_,_,D),D).
get_artist(track(_,A,_),A).

get_distance(artist_distance(_,_,D),D).

get_tent_index(Position, Tent) :-
  get_index(Position, Index),
  Index tent_get Tent.

%----------------------------------------------------------------------
% Shuffler
%----------------------------------------------------------------------

shuffler(Tracks) :-
  length(Tracks, N),
  length(Indices, N), % len(Tracks) is equal to len(Indices)
  tent_init(Indices), % set initial values for all index

  constraint_setup(Indices, Tracks, BSum, Distances), % BSum will keep track of the total cost for the sequence, the sum of distances to threshold
  hill_climb(Indices, BSum).

%----------------------------------------------------------------------
% Hill Climbing
%----------------------------------------------------------------------
hill_climb(Indices, BSum) :-
  conflict_constraints(cs, List),
  BSum tent_get OldCost,
  ( List=[] ->
    get_final_playback(Indices),
    writeln("DONE"),
    print_list(Indices)
  ;
    select_var(List, Var1),
    select_other_var(Indices, Var1, Var2),
    swap(Var1, Var2),
    BSum tent_get NewCost,
    write("New cost "), writeln(NewCost),
    ( NewCost < OldCost ->
      hill_climb(Indices, BSum)
    ;
      writeln("Local Optimum")
    )
  ).

select_var(List, Index) :-
  member(Constraint, List), % Choose one of the conflicting constraints
  arg(1, Constraint, Res),
  arg(2, Res, ArtistDistance),
  arg(1, ArtistDistance, Var),
  arg(1, Var, Index). % Get the first index involved in the constraint

select_other_var(Indices, Var1, Var2) :-
  member(Var2, Indices),
  Var1 \== Var2. % Choose a random index that isn't the constraint index

swap(Var1, Var2) :-
  Var1 tent_get Value1,
  Var2 tent_get Value2,
  Var1 tent_set Value2,
  Var2 tent_set Value1.

get_final_playback(List) :-
  List tent_get Tent,
  List = Tent.

%----------------------------------------------------------------------
% Constraint Setup
%----------------------------------------------------------------------
tent_init(List) :-
  length(List, N),
  ( for(I, 0, N-1), foreach(Var, List) do
    Var tent_set I
  ).

constraint_setup(Indices, Tracks, BSum, Distances) :-
  calculate_distances(Indices, Tracks, Distances),
  ( foreach(Dist, Distances), foreach(B, AllBs) do
    get_distance(Dist, D),
    arg(distance of artist_distance, Dist) $= 0 r_conflict cs,
    B tent_is D
  ),
  sumlist(AllBs, Sum),
  BSum tent_is Sum.


%----------------------------------------------------------------------
% Distance Calculation
%----------------------------------------------------------------------

calculate_distances(Indices, Tracks, Distances) :-
  get_playback(Indices, Tracks, Playback),
  ( foreach(P, Playback), foreach(D, Distances), param(Playback) do
      playback_tail(P, Playback, PlaybackTail),
      distance(P, PlaybackTail, Next, Distance), % Get the distance to the next track for same artist as well as that position
      get_tent_index(Next, NextPosition),
      ( NextPosition > -1 -> % Another track from the same artist was found
        threshold_diff(Distance, 8, Diff), % Diff is the difference from the Distance to 8, Diff >= 0
        D = artist_distance{position1: P, position2: Next, distance: Diff}
      ;
        D = artist_distance{position1: P, position2: -1, distance: 0}
      )
  ).

distance(Position, [H|_], H, 0) :-
  get_track(Position, T1),
  get_artist(T1, A1),

  get_track(H, T2),
  get_artist(T2, A2),

  T1 \== T2, % The tracks are not the same
  A1 == A2. % Position.Track and Head.Track have the same artist, now we stop calculate distance

distance(_, [], position{index: -1}, 999). % If we've reached end of list, there was no track with the same artist, set Next to -1

distance(Position, [_|T], Next, Distance) :-
  distance(Position, T, Next, Length),

  get_track(Position, Track),
  get_duration(Track, D),

  Distance is D + Length.

%----------------------------------------------------------------------
% Help Functions
%----------------------------------------------------------------------

get_playback(Indices, Tracks, SortedPlayback) :-
  (foreach(P, Playback), foreach(I, Indices), foreach(T, Tracks) do
    P = position{index: I, track: T}
  ),
  sort(1, <, Playback, SortedPlayback). % Sort Playback on index

threshold_diff(Distance, Threshold, Diff) :-
  ( (Threshold - Distance) > 0 ->
      Diff is (Threshold - Distance)
  ;
    Diff is 0
  ).

playback_tail(_,[],[]).

playback_tail(P, [H|T], T) :-
  P == H.

playback_tail(P, [_|T], Tail) :-
  playback_tail(P, T, Tail).

