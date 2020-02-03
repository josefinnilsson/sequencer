:- lib(ic).

num(0).
num(1).
num(2).
num(3).
num(4).
num(5).
num(6).
num(7).
num(8).
num(9).

fruitsLP(Apples, Bananas, Pears):-
  num(Apples),
  num(Bananas),
  num(Pears),
  Total is Apples * 6 + Pears * 7 + Bananas * 5,
  Amount is Apples + Pears + Bananas,
  Total = 96,
  Amount = 17.

fruitsCLP(Apples, Bananas, Pears):-
  [Apples, Pears, Bananas] :: 0..9,
  Apples + Pears + Bananas #=  17,
  Apples * 6 + Pears * 7 + Bananas * 5 #= 96,
  labeling([Apples, Pears, Bananas]).
