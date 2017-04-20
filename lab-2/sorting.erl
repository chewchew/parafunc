-module(sorting).     % module is named sorting (like filename)
-compile(export_all). % compile with options to export all functions

% Quicksort (sequential)
qsort([]) ->
  [];
qsort([X|Xs]) ->
  qsort([Y || Y <- Xs, Y < X]) ++
  [X] ++
  qsort([Y || Y <- Xs, Y >= X]).


% Helper for creating random lists of N integers
random_list(N) ->
  [rand:uniform(100000) || _ <- lists:seq(1,N)].

% Sorted list property
prop_sorted([]) -> 
  true;
prop_sorted([Cur,Next]) ->
  Cur =< Next;
prop_sorted([Cur,Next|Xs]) ->
  (Cur =< Next) and prop_sorted([Next|Xs]). 

% Quicksort property
prop_qsort(Xs) -> 
  prop_sorted(qsort(Xs)).
