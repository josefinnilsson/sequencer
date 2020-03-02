:- lib(repair).
:- lib(ic).
:- lib(ic_global).
:- import random_element/2 from tentative.


:- local struct(track(id, artist, duration)).
:- local struct(position(index, track, tent)).
:- local struct(artist_distance(position1, position2, distance)).

%----------------------------------------------------------------------
% Get Functions
%----------------------------------------------------------------------

get_index(position(I,_,_),I).
get_track(position(_,T,_),T).

get_duration(track(_,_,D),D).
get_artist(track(_,A,_),A).

get_distance(artist_distance(_,_,D),D).
get_first_position(artist_distance(P,_,_),P).
get_second_position(artist_distance(_,P,_),P).

get_id_from_position(position(_, track(ID, _, _), _), ID).

get_tent_index(Position, Tent) :-
  get_index(Position, Index),
  Index tent_get Tent.
get_tent_index(position(-1,_,_),-1).

%----------------------------------------------------------------------
% Shuffler
%----------------------------------------------------------------------

shuffler(Tracks) :-
  cputime(StartTime),

  length(Tracks, N),
  length(Indices, N), % len(Tracks) is equal to len(Indices)
  tent_init(Indices), % set initial values for all index

  constraint_setup(Indices, Tracks, BSum, Distances), % BSum will keep track of the total cost for the sequence, the sum of distances to threshold
  writeln("--- Starting Hill Climb ---"),
  hill_climb(Indices, Distances, Tracks, BSum),!,

  TimeUsed is cputime-StartTime,
  printf("Goal took %.2f seconds%n", [TimeUsed]).

%----------------------------------------------------------------------
% Hill Climbing
%----------------------------------------------------------------------
hill_climb(Indices, Distances, Tracks, BSum) :-
  conflict_constraints(cs, List),
  BSum tent_get OldCost,
  ( List=[] ->
    writeln("Playback:"),
    get_final_playback(Indices),
    get_playback(Indices, Tracks, Playback),
    print_list(Playback)
  ;
      select_var(List, Var1),
      select_other_var(Indices, Var1, Var2),
      swap(Var1, Var2),
      write("Swapped "), write(Var1), write(" and "), writeln(Var2),

      writeln("--- Previous distances ---"),
      print_list(Distances),

      update_distances(Indices, Tracks, Distances, Updated), %%Här måste det vara deterministisk

      writeln("--- Updated distances ---"),
      print_list(Updated),

      BSum tent_get NewCost,

      write("Old cost: "), write(OldCost), write(" new cost: "), writeln(NewCost),

      NewCost < OldCost,

      hill_climb(Indices, Updated, Tracks, BSum)
  ).

select_var(List, Index) :-
  member(Constraint, List), % Choose one of the conflicting constraints
  arg(1, Constraint, Res),
  arg(2, Res, ArtistDistance),
  arg(1, ArtistDistance, Var),
  arg(1, Var, Index). % Get the first index involved in the constraint

select_other_var(Indices, Var1, Var2) :-
  random_element(Indices, Var2),
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
    arg(distance of artist_distance, Dist) $= 0 r_conflict cs,
    tent_call([Dist], BDist, BDist is arg(distance of artist_distance, Dist)),
    B tent_is BDist
  ),
  tent_call([AllBs], BSum, BSum is sumlist(AllBs)).


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
        threshold_diff(Distance, 14, Diff), % Diff is the difference from the Distance to 8, Diff >= 0
        TentDiff tent_set Diff,
        D = artist_distance{position1: P, position2: Next, distance: TentDiff}
      ;
        NewDist tent_set 0,
        D = artist_distance{position1: P, position2: Next, distance: NewDist} % This causes problems
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

update_distances(Indices, Tracks, FilteredDistances, Updated) :-
  get_playback(Indices, Tracks, Playback), %% Den här är rätt
  ( foreach(D, FilteredDistances), foreach(UD, Updated), param(Playback) do
    %% get_smallest_position(D, Position), %% Om artist_distance bara innehåller en låt måste den returneras
    get_first_position(D, Position),
    recalculate(Position, Playback, Distance, Next),!,
    get_distance(D, Dis),
    Dis tent_set Distance,
    UD = artist_distance{position1: Position, position2: Next, distance: Dis}
  ).

recalculate(Position, Playback, Distance, NextP) :-
  playback_tail(Position, Playback, PlaybackTail), %% Den här returnerar fel för de som har blivit swappade
  distance(Position, PlaybackTail, Next, Dist),
  get_tent_index(Next, NextPosition),
  ( NextPosition > -1 -> % Another track from the same artist was found
      threshold_diff(Dist, 14, Diff), % Diff is the difference from the Distance to 8, Diff >= 0
      Distance = Diff,
      NextP = Next
    ;
      Distance = 0,
      NextP = position{index: -1}
  ).

%----------------------------------------------------------------------
% Helper Functions
%----------------------------------------------------------------------


get_smallest_position(ArtistDistance, Position) :-
  get_first_position(ArtistDistance, First),
  get_index(First, FirstI),
  get_second_position(ArtistDistance, Second),
  get_index(Second, SecondI),
  FirstI tent_get FirstT,
  SecondI tent_get SecondT,
  (FirstT =< SecondT ->
    ( FirstT >= 0 ->
      Position = First
      ;
      Position = Second
    )
    ;
    ( SecondT >= 0 ->
      Position = Second
      ;
      Position = First
    )
  ).

get_playback(Indices, Tracks, SortedPlayback) :-
  (foreach(P, Playback), foreach(I, Indices), foreach(T, Tracks) do
    I tent_get Tent,
    P = position{index: I, track: T, tent: Tent}
  ),
  sort(3, <, Playback, SortedPlayback). % Sort Playback on index

threshold_diff(Distance, Threshold, Diff) :-
  ( (Threshold - Distance) > 0 ->
      Diff is (Threshold - Distance)
  ;
    Diff is 0
  ).

playback_tail(_,[],[]).

playback_tail(P, [H|T], T) :-
  get_id_from_position(P, ID1),
  get_id_from_position(H, ID2),
  ID1 == ID2.

playback_tail(P, [_|T], Tail) :-
  playback_tail(P, T, Tail).


get_artist_distance_from_index([H|_], Var, H) :-
  get_first_position(H, First),
  get_index(First, P1),
  P1 == Var.

get_artist_distance_from_index([H|_], Var, H) :-
  get_second_position(H, Second),
  get_index(Second, P2),
  P2 == Var.

get_artist_distance_from_index([_|T], Var, AD) :-
  get_artist_distance_from_index(T, Var, AD).

add(X,[],[X]).
add(X,[Y|Tail],[Y|Tail1]):-
  add(X,Tail,Tail1).

