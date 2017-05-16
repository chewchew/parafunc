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
	process_flag(trap_exit,true),
	lists:flatten(lists:map(
			fun(Node) -> 
				[spawn_link(Node,
					fun() ->
						dets:open_file(web,[{file,"web.dat"}]),
						worker(Master,Pool) 
					end) || _ <- lists:seq(1,NWorkers)] 
			end, Nodes)).

start(Jobs) ->
	Master = self(),
	Nodes  = nodes(),
	Pool = start_job_pool(Jobs),
	Workers = init_workers(Master,Pool,Nodes,?N_WORKERS),
	lists:filter(fun(X) -> case X of sig -> false; _ -> true end end,[receive 
			{JobId,R} -> R;
			{'EXIT',From,normal} ->
				io:format("Node ~w finished with no problem.~n",[From]),
				sig;
			{'EXIT',From,Reason} -> 
				io:format("Node ~w ~w, running job on main thread.~n",[From,Reason]),
				spawn(backup,worker,[Master,Pool]),
				sig
		end || {JobId,_} <- Jobs]).