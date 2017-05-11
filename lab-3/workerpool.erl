-module(workerpool).
-compile(export_all).

-define(N_WORKERS,4).

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
		{exit,From} -> From ! ok
	end.

worker(Master,Pool) ->
	Pool ! {get_job,self()},
	receive
		no_jobs -> done;
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