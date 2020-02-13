:- lib(ic).
:- lib(ic_search).

consecArtist([]).
consecArtist([_]).
consecArtist([X,Y|T]) :-
  A1 is getArtist(X),
  A2 is getArtist(Y),
  A1 \== A2,
  consecArtist([Y|T]),!.

bpmDiff([]).
bpmDiff([_]).
bpmDiff([X,Y|T]) :-
  BPM1 is getBPM(X),
  BPM2 is getBPM(Y),
  abs(BPM1-BPM2) =< 7,
  bpmDiff([Y|T]).

shuffle([],[]).
shuffle(L, [H|T]) :-
  append(V,[H|U],L),
  append(V,U,W),
  shuffle(W,T).

getID(track(ID,_,_),ID).
getBPM(track(_,_,B), B).
getArtist(track(_,A,_), A).

generateSequence(L1, L2) :-
  shuffle(L1, L2),
  consecArtist(L2),
  bpmDiff(L2).

sequencer(Tracks) :-
  shuffle(Tracks, Perm),
  consecArtist(Perm),
  bpmDiff(Perm),
  search(Perm, 3, input_order, indomain, complete, []),
  writeln(Perm),
  fail.

readData(_S,end_of_file,[]) :- !.
readData(S,X,[X|R]) :-
        read(S,Y),
        readData(S,Y,R).

:-mode output_data(++,+).
outputData(File,L):-
        open(File,'write',S),
        (foreach(X,L),
         param(S) do
           writeq(S,X),writeln('.')
        ),
        close(S).

sequence(Tracks, Seq) :-
  open(Tracks,read,S),
  read(S,X),
  readData(S,X,Res),
  close(S),
  generateSequence(Res,Seq).






