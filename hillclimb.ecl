%----------------------------------------------------------------------
% Libraries
%----------------------------------------------------------------------

:- lib(repair).
:- lib(ic).
:- lib(ic_global).
:- import random_element/2 from tentative.


%----------------------------------------------------------------------
% Structs
%----------------------------------------------------------------------

:- local struct(track(id, artist, duration, genre)).
:- local struct(position(index, track, tent)).
:- local struct(artist_distance(position1, position2, distance)).
:- local struct(succ_genre(position1, position2, different)).

%----------------------------------------------------------------------
% Get Functions
%----------------------------------------------------------------------

get_index(position(I,_,_),I).
get_track(position(_,T,_),T).

get_duration(track(_,_,D,_),D).
get_artist(track(_,A,_,_),A).

get_distance(artist_distance(_,_,D),D).
get_first_position(artist_distance(P,_,_),P).
get_second_position(artist_distance(_,P,_),P).
get_first_from_genre(succ_genre(P,_,_),P).
get_second_from_genre(succ_genre(_,P,_),P).
get_different(succ_genre(_,_,D),D).

get_id_from_position(position(_, track(ID, _, _, _), _), ID).

get_tent_index(Position, Tent) :-
  get_index(Position, Index),
  Index tent_get Tent.
get_tent_index(position(-1,_,_),-1).

%----------------------------------------------------------------------
% Shuffler
%----------------------------------------------------------------------

shuffler(Tracks, ArtistDistance, Result, Cost, TimeUsed) :-
  cputime(StartTime),

  length(Tracks, N),
  length(Indices, N), % len(Tracks) is equal to len(Indices)
  tent_init(Indices), % set initial values for all index

  constraint_setup(Indices, Tracks, BSum, ArtistDistance, Distances), % BSum will keep track of the total cost for the sequence, the sum of distances to threshold
  genre_constraint_setup(Indices, Tracks, GSum, GenrePairs),
  tent_call([GSum, BSum], TotalSum, TotalSum is BSum+GSum),

  hill_climb(Indices, Distances, GenrePairs, Tracks, TotalSum, ArtistDistance, 0, 10, 9999, Indices, Result, Cost),!,

  TimeUsed is cputime-StartTime.

final(Indices, Tracks, BestCost, Result, Cost):- % When the final playback is found
  get_final_playback(Indices, Tentative),
  get_playback(Tentative, Tracks, Result),
  Cost is BestCost.

get_final_playback(List, Tentative) :-
  ( foreach(L, List), foreach(T, Tentative) do
    L tent_get T
  ).

is_done([], _, true).
is_done(_, 0, true).
is_done(_, _, false).

%----------------------------------------------------------------------
% Hill Climbing
%----------------------------------------------------------------------

hill_climb(Indices, Distances, GenrePairs, Tracks, TotalSum, ArtistDistance, Count, Max, BestCost, BestIndices, Result, Cost) :-
  conflict_constraints(cs, List), % List will include current conflicting constraints
  TotalSum tent_get OldSum, % Store the old cost
  is_done(List, OldSum, Done),
  ( Done == true -> % If the cost is 0, an optimal solution is found
    final(Indices, Tracks, 0, Result, Cost)
  ;
      select_var(List, Var1), % Choose an arbitrary variable from an arbitrary conflicting constraint
      select_other_var(Indices, Var1, Var2), % Select a random variable to swap with
      swap(Var1, Var2), % Swap the two variables

      NewCount is Count + 1, % Increment the count once
      ( NewCount > Max ->
        final(BestIndices, Tracks, BestCost, Result, Cost) % If no more tries are allowed, return solution
        ;
        update_distances(Indices, Tracks, Distances, ArtistDistance, Updated), % Recalculate the distances, Updated holds the new Distnaces

        update_genres(Indices, Tracks, GenrePairs, Var1, Var2, GenrePairsUpdated),

        TotalSum tent_get NewSum, % Get the new cost

        NewCount2 is NewCount + 1, % Increment the count again (because of multiple swaps) TODO: Make cleaner

        (NewCount2 > Max ->
          final(BestIndices, Tracks, BestCost, Result, Cost) % If no more tries are allowed, return solution
          ;
          NewSum < OldSum,

          ( NewSum < BestCost ->
            hill_climb(Indices, Updated, GenrePairsUpdated, Tracks, TotalSum, ArtistDistance,  NewCount2, Max, NewSum, Indices, Result, Cost) % Move on with the new order as the best
          ;
            hill_climb(Indices, Updated, GenrePairsUpdated, Tracks, TotalSum, ArtistDistance, NewCount2, Max, BestCost, BestIndices, Result, Cost) % Move on with the old order as the best
          )
        )
      )
  ).


%----------------------------------------------------------------------
% Swapping
%----------------------------------------------------------------------

select_var(List, Index) :-
  member(Constraint, List), % Choose one of the conflicting constraints
  arg(1, Constraint, Res),
  arg(2, Res, ArtistDistance),
  arg(1, ArtistDistance, Var),
  arg(1, Var, Index). % Get the first index involved in the constraint

select_other_var(Indices, Var1, Var2) :- % TODO: Optimise
  random_element(Indices, Var2),
  Var1 \== Var2. % Choose a random index that isn't the constraint index

swap(Var1, Var2) :-
  Var1 tent_get Value1,
  Var2 tent_get Value2,
  Var1 tent_set Value2,
  Var2 tent_set Value1.

%----------------------------------------------------------------------
% Constraint Setup
%----------------------------------------------------------------------

tent_init(List) :- % Create an initial playback with order 0,1,..N
  length(List, N),
  ( for(I, 0, N-1), foreach(Var, List) do
    Var tent_set I
  ).

constraint_setup(Indices, Tracks, BSum, ArtistDistance, Distances) :- % Initialise the constraints
  calculate_distances(Indices, Tracks, ArtistDistance, Distances),
  ( foreach(Dist, Distances), foreach(B, AllBs) do
    arg(distance of artist_distance, Dist) $= 0 r_conflict cs,
    tent_call([Dist], BDist, BDist is arg(distance of artist_distance, Dist)),
    B tent_is BDist
  ),
  tent_call([AllBs], BSum, BSum is sumlist(AllBs)).

genre_constraint_setup(Indices, Tracks, GSum, GenrePairs) :-
  calculate_genres(Indices, Tracks, GenrePairs),
  ( foreach(Pair, GenrePairs), foreach(Score, AllScores) do
    arg(different of succ_genre, Pair) $= 0 r_conflict cs2,
    tent_call([Pair], DiffScore, DiffScore is arg(different of succ_genre, Pair)),
    Score tent_is DiffScore
  ),
  tent_call([AllScores], GSum, GSum is sumlist(AllScores)).

%----------------------------------------------------------------------
% Genre Calculation
%----------------------------------------------------------------------
calculate_genres(Indices, Tracks, GenrePairs) :-
  get_playback(Indices, Tracks, Playback),
  ( foreach(P, Playback), foreach(Pair, GenrePairs), param(Playback) do
    is_different(P, Playback, Next, Different),
    ( Different == true ->
      TentDifferent tent_set 0,
      Pair = succ_genre{position1: P, position2: Next, different: TentDifferent} % If they're different
      ;
      TentDifferent tent_set 1,
      Pair = succ_genre{position1: P, position2: Next, different: TentDifferent} % If they're the same
    )
  ).

is_different(P, Playback, Next, Different) :-
  get_next(P, Playback, Next),
  ( P == Next ->
    Different = true
    ;
    get_track(P, Track),
    get_track(Next, NextTrack),
    arg(genre of track, Track, Genre),
    arg(genre of track, NextTrack, NextGenre),
    ( Genre $\= NextGenre ->
      Different = true
    ;
      Different = false
    )
  )
.

get_next(P, [P2], P):-
  get_track(P, T1),
  get_track(P2, T2),
  T1 == T2.

get_next(P, [P2,Y|_], Y):-
  get_track(P, T1),
  get_track(P2, T2),
  T1 == T2.

get_next(P, [], P).

get_next(P, [_|T], Y) :- get_next(P,T,Y).

%----------------------------------------------------------------------
% Distance Calculation
%----------------------------------------------------------------------

calculate_distances(Indices, Tracks, ArtistDistance, Distances) :-
  get_playback(Indices, Tracks, Playback),
  ( foreach(P, Playback), foreach(D, Distances), param(Playback, ArtistDistance) do
      playback_tail(P, Playback, PlaybackTail),
      distance(P, PlaybackTail, Next, Distance), % Get the distance to the next track for same artist as well as that position
      get_tent_index(Next, NextPosition),
      ( NextPosition > -1 -> % Another track from the same artist was found
        threshold_diff(Distance, ArtistDistance, Diff), % Diff is the difference from the Distance to 8, Diff >= 0
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

update_distances(Indices, Tracks, Distances, ArtistDistance, Updated) :- % Recalculate the distances for the current playback
  get_playback(Indices, Tracks, Playback),
  ( foreach(D, Distances), foreach(UD, Updated), param(Playback, ArtistDistance) do
    get_first_position(D, Position),
    recalculate(Position, Playback, ArtistDistance, Distance, Next),!,
    get_distance(D, Dis),
    Dis tent_set Distance,
    UD = artist_distance{position1: Position, position2: Next, distance: Dis}
  ).

recalculate(Position, Playback, ArtistDistance, Distance, NextP) :-
  playback_tail(Position, Playback, PlaybackTail),
  distance(Position, PlaybackTail, Next, Dist),
  get_tent_index(Next, NextPosition),!,
  ( NextPosition > -1 -> % Another track from the same artist was found
      threshold_diff(Dist, ArtistDistance, Diff), % Diff is the difference from the Distance to 8, Diff >= 0
      Distance = Diff,
      NextP = Next
    ;
      Distance = 0,
      NextP = position{index: -1} % If there are no more tracks from this artist, set index to -1
  ).

equal(Var1,_,X,true) :-
  Var1 tent_get T1,
  X tent_get T2,
  T1 == T2.
equal(_,Var2,X,true) :-
  Var2 tent_get T1,
  X tent_get T2,
  T1 == T2.
equal(_,_,_,false).

update_genres(Indices, Tracks, GenrePairs, Var1, Var2, GenrePairsUpdated) :-
  get_playback(Indices, Tracks, Playback),
  ( foreach(Pair, GenrePairs), foreach(UP, GenrePairsUpdated), param(Playback, Var1, Var2) do
    get_first_from_genre(Pair, First),
    is_different(First, Playback, Next, Different),!, % TODO: Gör deteministisk
    ( Different == true ->
        get_different(Pair, Dif),
        Dif tent_set 0,
        UP = succ_genre{position1: First, position2: Next, different: Dif}
        ;
        get_different(Pair, Dif),
        Dif tent_set 1,
        UP = succ_genre{position1: First, position2: Next, different: Dif}
    )
  )
.


%----------------------------------------------------------------------
% Helper Functions
%----------------------------------------------------------------------

get_playback(Indices, Tracks, SortedPlayback) :-
  (foreach(P, Playback), foreach(I, Indices), foreach(T, Tracks) do
    I tent_get Tent,
    P = position{index: I, track: T, tent: Tent}
  ),
  sort(3, <, Playback, SortedPlayback). % Sort Playback on index

threshold_diff(Distance, Threshold, Diff) :- % Return the difference between threshold and distance between two artists
  ( (Threshold - Distance) > 0 ->
      Diff is (Threshold - Distance)
  ;
    Diff is 0
  ).

playback_tail(_,[],[]). % playback_tail returns the succeeding tracks in the playback given a single track

playback_tail(P, [H|T], T) :-
  get_id_from_position(P, ID1),
  get_id_from_position(H, ID2),
  ID1 == ID2.

playback_tail(P, [_|T], Tail) :-
  playback_tail(P, T, Tail).
