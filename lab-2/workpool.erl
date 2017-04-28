% Taken from the exercise session with Anton
-module (workpool).
-compile (export_all).

start_pool(N) ->
    true = register(pool,spawn_link(fun()->pool([worker() || _ <- lists:seq(1,N)]) end)).

pool(Workers) ->
    pool(Workers,Workers).

pool(Workers,All) ->
    receive
	{get_worker,Pid} ->
	    case Workers of
		[] ->
		    Pid ! {pool,no_worker},
		    pool(Workers,All);
		[W|Ws] ->
		    Pid ! {pool,W},
		    pool(Ws,All)
	    end;
	{return_worker,W} ->
	    pool([W|Workers],All);
	{stop,Pid} ->
	    [unlink(W) || W <- All],
	    [exit(W,kill) || W <- All],
	    unregister(pool),
	    Pid ! {pool,stopped}
    end.

worker() ->
    spawn_link(fun work/0).

work() ->
    receive
	{task,Pid,R,F} ->
	    Pid ! {R,F()},
	    pool ! {return_worker,self()},
	    work()
    end.

speculate_on_worker(F) ->
    case whereis(pool) of
		undefined ->
		    ok; %% we're stopping
		Pool -> Pool ! {get_worker,self()}
    end,
    receive
		{pool,no_worker} ->
		    {not_speculating,F};
		{pool,W} ->
		    R = make_ref(),
		    W ! {task,self(),R,F},
		    {speculating,R}
    end.

worker_value_of({not_speculating,F}) ->
    F();
worker_value_of({speculating,R}) ->
    receive
	{R,X} ->
	    X
    end.
