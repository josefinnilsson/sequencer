:- lib(repair).
:- lib(ic).

prop_sat(Vars) :-
  Vars = [X1, X2, X3],
  tent_init(Vars),
  (X1 or neg X3 or X3 $= 1) r_conflict cs,
  (neg X1 or neg X2 $= 1) r_conflict cs,
  (X2 or neg X3 $= 1) r_conflict cs,
  min_conflicts(Vars).

tent_init(List) :-
  ( foreach(Var, List) do
    Var tent_set 1
  ).

local_search(Vars) :-
  conflict_constraints(cs, List),
  ( List = [] ->
    set_to_tent(Vars)
    ;
    List = [Constraint|_] ->
      term_variables(Constraint,[Var|_]),
      move(Var),
      local_search(Vars)
  ).

move(Var) :-
  Var tent_get Value,
  NewValue is (1-Value),
  var tent_set NewValue.

set_to_tent(Term) :-
   Term tent_get Tent,
   Term = Tent.