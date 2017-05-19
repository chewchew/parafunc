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

% Main entry-point, takes a list of Jobs and 
% calls all initiation functions
start(Jobs) ->
	Master = self(),
	Nodes  = nodes(),
	Pool = start_job_pool(Jobs),
	Workers = init_workers(Master,Pool,Nodes,?N_WORKERS),
	lists:filter(fun(X) -> 
		case X of 
			exit -> false; 
			_ -> true 
		end 
	end,
	[receive 
		{JobId,R} -> R;
		{'EXIT',From,normal} ->
			io:format("Node ~w finished with no problem.~n",[From]),
			exit;
		{'EXIT',From,Reason} -> 
			WorkerPid = spawn_link(fun() -> worker(Master,Pool) end),
			io:format("Node ~w ~w, running job on main thread as ~w.~n",
				[From,Reason,WorkerPid]),
			exit
	end || {JobId,_} <- Jobs]).