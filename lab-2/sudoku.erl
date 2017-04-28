-module(sudoku).
%-include_lib("eqc/include/eqc.hrl").
-import (workpool, [start_pool/1,speculate_on_worker/1,worker_value_of/1]).
-compile(export_all).

%% %% generators

%% matrix(M,N) ->
%%     vector(M,vector(N,nat())).

%% matrix transpose
% low prio parallel
transpose([Row]) ->
    [[X] || X <- Row];
transpose([Row|M]) ->
    [[X|Xs] || {X,Xs} <- lists:zip(Row,transpose(M))].

%% prop_transpose() ->
%%     ?FORALL({M,N},{nat(),nat()},
%% 	    ?FORALL(Mat,matrix(M+1,N+1),
%% 		    transpose(transpose(Mat)) == Mat)).

%% map a matrix to a list of 3x3 blocks, each represented by the list
%% of elements in row order
% low prio parallel
triples([A,B,C|D]) ->
    [[A,B,C]|triples(D)];
triples([]) ->
    [].

% low prio paralell
blocks(M) ->
    Blocks = [triples(X) || X <- transpose([triples(Row) || Row <- M])],
    lists:append(
      lists:map(fun(X)->
			lists:map(fun lists:append/1, X)
		end,
		Blocks)).

% low prio paralell
unblocks(M) ->
    lists:map(
      fun lists:append/1,
      transpose(
	lists:map(
	  fun lists:append/1,
	  lists:map(
	    fun(X)->lists:map(fun triples/1,X) end,
	    triples(M))))).

%% prop_blocks() ->
%%     ?FORALL(M,matrix(9,9),
%% 	    unblocks(blocks(M)) == M).

%% decide whether a position is safe

entries(Row) ->
    [X || X <- Row,
	  1 =< X andalso X =< 9].

safe_entries(Row) ->
    Entries = entries(Row),
    lists:sort(Entries) == lists:usort(Entries).

% med prio paralell
safe_rows(M) ->
    lists:all(fun safe_entries/1,M).

safe(M) ->
    safe_rows(M) andalso
	safe_rows(transpose(M)) andalso
	safe_rows(blocks(M)).

%% fill blank entries with a list of all possible values 1..9

fill(M) ->
    Nine = lists:seq(1,9),
    [[if 1=<X, X=<9 ->
	      X;
	 true ->
	      Nine
      end
      || X <- Row]
     || Row <- M].

%% refine entries which are lists by removing numbers they are known
%% not to be

refine(M) ->
    NewM =
	refine_rows(
	  transpose(
	    refine_rows(
	      transpose(
			unblocks(
			  refine_rows(
			    blocks(M))))))),
    if M==NewM ->
	    M;
       true ->
	    refine(NewM)
    end.

parmap(F, Ls) ->
	Parent = self(),
	Refs = lists:map(fun(X) -> spawn(fun() ->
		case catch F(X) of
			{'EXIT',_} -> Parent ! {self(),{'EXIT',no_solution}};
			Res -> Parent ! {self(), Res}
		end
		end) end, Ls),
	lists:map(fun(Ref) -> receive {Ref,Val} -> Val end end, Refs).

refine_rows(M) ->
	% Parallel refine_rows:
    % parmap(fun refine_row/1,M).

    % Sequential refine_rows:
    lists:map(fun refine_row/1,M).

refine_row(Row) ->
    Entries = entries(Row),
    NewRow =
	[if is_list(X) ->
		 case X--Entries of
		     [] ->
			 exit(no_solution);
		     [Y] ->
			 Y;
		     NewX ->
			 NewX
		 end;
	    true ->
		 X
	 end
	 || X <- Row],
    NewEntries = entries(NewRow),
    %% check we didn't create a duplicate entry
    case length(lists:usort(NewEntries)) == length(NewEntries) of
	true ->
	    NewRow;
	false ->
	    exit(no_solution)
    end.

is_exit({'EXIT',_}) ->
    true;
is_exit(_) ->
    false.

%% is a puzzle solved?

solved(M) ->
    lists:all(fun solved_row/1,M).

solved_row(Row) ->
    lists:all(fun(X)-> 1=<X andalso X=<9 end, Row).

%% how hard is the puzzle?

hard(M) ->		      
    lists:sum(
      [lists:sum(
	 [if is_list(X) ->
		  length(X);
	     true ->
		  0
	  end
	  || X <- Row])
       || Row <- M]).

%% choose a position {I,J,Guesses} to guess an element, with the
%% fewest possible choices

guess(M) ->
    Nine = lists:seq(1,9),
    {_,I,J,X} =
	lists:min([{length(X),I,J,X}
		   || {I,Row} <- lists:zip(Nine,M),
		      {J,X} <- lists:zip(Nine,Row),
		      is_list(X)]),
    {I,J,X}.

%% given a matrix, guess an element to form a list of possible
%% extended matrices, easiest problem first.

guesses(M) ->
    {I,J,Guesses} = guess(M),
    Ms = [catch refine(update_element(M,I,J,G)) || G <- Guesses],
    SortedGuesses =
	lists:sort(
	  [{hard(NewM),NewM}
	   || NewM <- Ms,
	      not is_exit(NewM)]),
    [G || {_,G} <- SortedGuesses].

update_element(M,I,J,G) ->
    update_nth(I,update_nth(J,G,lists:nth(I,M)),M).

update_nth(I,X,Xs) ->
    {Pre,[_|Post]} = lists:split(I-1,Xs),
    Pre++[X|Post].

%% prop_update() ->
%%     ?FORALL(L,list(int()),
%% 	    ?IMPLIES(L/=[],
%% 		     ?FORALL(I,choose(1,length(L)),
%% 			     update_nth(I,lists:nth(I,L),L) == L))).

%% solve a puzzle

solve(M) -> 
    Solution = solve_refined(refine(fill(M))),
	case valid_solution(Solution) of
		true ->
		    Solution;
		false ->
		    exit({invalid_solution,Solution})
    end.

solve_refined(M) ->
    case solved(M) of
	true ->
	    M;
	false ->
	    solve_one(guesses(M))
    end.

solve_one([]) ->
    exit(no_solution);
solve_one([M]) ->
    solve_refined(M);
solve_one([M|Ms]) ->
    case catch solve_refined(M) of
	{'EXIT',no_solution} ->
	    solve_one(Ms);
	Solution ->
	    Solution
    end.

% solved using parmap over the guesses
solve_pmap(M) ->
    Solution = solve_pmap_parallel(6,refine(fill(M))),
	case valid_solution(Solution) of
		true ->
		    Solution;
		false ->
		    exit({invalid_solution,Solution})
    end.

solve_pmap_parallel(0,M) -> solve_refined(M);
solve_pmap_parallel(D,M) -> 
	case solved(M) of
		true  -> M;
		false -> solve_pmap_rec(D,guesses(M))
	end.

solve_pmap_rec(_,[]) -> exit(no_solution);
solve_pmap_rec(D,[M]) -> solve_pmap_parallel(D-1,M);
solve_pmap_rec(D,Ms) -> 
	Answers = parmap(fun(M) -> solve_pmap_parallel(D-1,M) end,Ms),
	Solutions = lists:filter(fun(Answer) -> not is_exit(Answer) end, Answers),
	hd(Solutions).

% solved using a worker pool
solve_pool(M) ->
    start_pool(erlang:system_info(schedulers)-1),
    Solution = solve_pool_parallel(refine(fill(M))),
    pool ! {stop,self()},
    receive {pool,stopped} -> case valid_solution(Solution) of
		true ->
		    Solution;
		false ->
		    exit({invalid_solution,Solution})
	    end
	end.

solve_pool_parallel(M) -> 
	case solved(M) of
		true  -> M;
		false -> solve_pool_rec(guesses(M))
	end.

solve_pool_rec([]) -> false;
solve_pool_rec([M|Ms]) ->
	Pid = speculate_on_worker(fun() -> solve_pool_parallel(M) end),
	case solve_pool_rec(Ms) of
		false ->  worker_value_of(Pid);
		Solution -> Solution
	end.

% solved using a worker pool and doing sequential execution on puzzles easier than 100
solve_pool1(M) ->
    start_pool(erlang:system_info(schedulers)-1),
    Solution = solve_pool1_parallel(refine(fill(M))),
    pool ! {stop,self()},
    receive {pool,stopped} -> case valid_solution(Solution) of
		true ->
		    Solution;
		false ->
		    exit({invalid_solution,Solution})
	    end
	end.

solve_pool1_parallel(M) -> 
	case solved(M) of
		true  -> M;
		false -> solve_pool1_rec(guesses(M))
	end.

solve_pool1_rec([]) -> false;
solve_pool1_rec([M|Ms]) ->
	Hard = hard(M),
	Pid =
		% The idea: If the puzzle is too easy then run it on one thread (job getting too small)
		if
		 	Hard >= 120 -> speculate_on_worker(fun() -> solve_pool1_parallel(M) end);
		 	true -> speculate_on_worker(fun() -> 
		 			case catch solve_refined(M) of 
		 				{'EXIT',_} -> false;
		 				Solution -> Solution
		 			end
		 		end)
	 	end,
	case solve_pool1_rec(Ms) of
		false ->  worker_value_of(Pid);
		Solution -> Solution
	end.

% solved using a worker pool and doing sequential execution on small guesses
solve_pool2(M) ->
    start_pool(erlang:system_info(schedulers)-1),
    Solution = solve_pool2_parallel(refine(fill(M))),
    pool ! {stop,self()},
    receive {pool,stopped} -> case valid_solution(Solution) of
		true ->
		    Solution;
		false ->
		    exit({invalid_solution,Solution})
	    end
	end.

solve_pool2_parallel(M) -> 
	case solved(M) of
		true  -> M;
		false -> solve_pool2_rec(guesses(M))
	end.

solve_pool2_rec([]) -> false;
% if we only have one guess run a sequential version
solve_pool2_rec(Ms) when length(Ms) < 2 ->
	case catch solve_one(Ms) of
		{'EXIT',_} -> false;
		Solution -> Solution
	end;
solve_pool2_rec([M|Ms]) ->
	Pid = speculate_on_worker(fun() -> solve_pool2_parallel(M) end),
	case solve_pool2_rec(Ms) of
		false -> worker_value_of(Pid);
		Solution -> Solution
	end.

%% benchmarks

-define(EXECUTIONS,10).

bm(F) ->
    {T,_} = timer:tc(?MODULE,repeat,[F]),
    T/?EXECUTIONS/1000.

repeat(F) ->
    [F() || _ <- lists:seq(1,?EXECUTIONS)].

benchmarks(Puzzles) ->
    % Parallel
	% Self = self(),
	% Pids = [ spawn_link(
	% 	fun() -> 
	% 		Self ! { self(), {Name,bm( fun()-> solve(M) end )} }
	% 	end) || {Name,M} <- Puzzles ],
	% [ receive {Pid,R} -> R end || Pid <- Pids ].

	% Sequential solve:
    [ {Name, bm( fun()-> solve(M) end )} || {Name,M} <- Puzzles].

    % Parallel solve's:
    % [ {Name, bm( fun()-> solve_pmap(M) end )} || {Name,M} <- Puzzles].
    % [ {Name, bm( fun()-> solve_pool(M) end )} || {Name,M} <- Puzzles].
    % [ {Name, bm( fun()-> solve_pool1(M) end )} || {Name,M} <- Puzzles].
    % [ {Name, bm( fun()-> solve_pool2(M) end )} || {Name,M} <- Puzzles].

benchmarks() ->
  {ok,Puzzles} = file:consult("problems.txt"),
  timer:tc(?MODULE,benchmarks,[Puzzles]).
		      
%% check solutions for validity

valid_rows(M) ->
    lists:all(fun valid_row/1,M).

valid_row(Row) ->
    lists:usort(Row) == lists:seq(1,9).

valid_solution(M) ->
    valid_rows(M) andalso valid_rows(transpose(M)) andalso valid_rows(blocks(M)).

