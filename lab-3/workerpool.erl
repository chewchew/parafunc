-module(workerpool).
-compile(export_all).

-define(EXECUTIONS,10).
-define(N_JOBS,500000).
-define(N_SPLITS,50).
-define(N_WORKERS,4).

add(X,Y) ->
	X + Y.

mul(X,Y) ->
	X * Y.

start_job_pool(Jobs) ->
	case whereis(job_pool) of
		undefined -> ok;
		Pool -> 
			Pool ! {exit,self()},
			receive
				ok -> ok
			end
	end,
	register(job_pool,spawn(workerpool,job_pool,[Jobs])),
	whereis(job_pool).

job_pool(Jobs) ->
	receive
		{get_job,Worker} ->
			case Jobs of
				[J|Js] ->
					% io:format("~w->~w~n",[Worker,J]),
					Worker ! J,
					job_pool(Js);
				[] ->
					Worker ! no_jobs,
					job_pool(Jobs)
			end;
		{new_pool,Js} -> job_pool(Js);
		{exit,From} -> From ! ok
	end.

worker(Master,Pool) ->
	% io:format("~w@~w~n",[self(),node()]),
	Pool ! {get_job,self()},
	receive
		no_jobs -> worker(Master,Pool);
		{Id,F} -> 
			Master ! {Id,F()},
			worker(Master,Pool)
	end.

init_workers(Master,Pool,Nodes,NWorkers) ->
	lists:flatten(lists:map(
			fun(Node) -> 
				[spawn(Node,
					fun() -> 
						worker(Master,Pool) 
					end) || _ <- lists:seq(1,NWorkers)] 
			end, Nodes)).

start(Jobs) ->
	Master = self(),
	Nodes  = nodes(),
	Pool = start_job_pool(Jobs),
	Workers = init_workers(Master,Pool,Nodes,?N_WORKERS),
	[receive {JobId,R} -> R end || {JobId,_} <- Jobs].
	% io:format("~w~n", [lists:sum([receive {JobId,R} -> lists:sum(R) end || {JobId,_} <- Jobs])]).

start_seq(Jobs) ->
	% io:format("~w~n",[lists:sum(lists:map(fun({_,F}) -> F() end, Jobs))]).
	lists:map(fun({_,F}) -> F() end, Jobs).

repeat(F) ->
    [F() || _ <- lists:seq(1,?EXECUTIONS)].

bm(F) ->
    {T,_} = timer:tc(?MODULE,repeat,[F]),
    T/?EXECUTIONS/1000.

benchmark_seq() ->
	bm(fun() -> start_seq(jobs(?N_JOBS)) end)/1000.

benchmark_par() ->
	bm(fun() -> start(jobs_par(?N_JOBS)) end)/1000.

split_into(N,L) ->
    split_into(N,L,length(L)).

split_into(1,L,_) ->
    [L];
split_into(N,L,Len) ->
    {Pre,Suf} = lists:split(Len div N,L),
    [Pre|split_into(N-1,Suf,Len-(Len div N))].

numbers(N) -> [{rand:uniform(200),rand:uniform(200)} || _ <- lists:seq(1,N)].

jobs(N) ->
	Numbers = numbers(N),
	[{I,fun() -> add(X,Y) end} || {I,{X,Y}} <- lists:zip(lists:seq(1,length(Numbers)),Numbers)].

jobs_par(N) -> 
	Numbers = numbers(N),
	Splits  = split_into(?N_SPLITS,Numbers),
	[
		{I,	fun() -> lists:map(fun({X,Y}) -> add(X,Y) end,Split) end} 
			|| 
		{I,Split} <- lists:zip(lists:seq(1,length(Splits)),Splits)
	].