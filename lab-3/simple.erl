-module(simple).
-compile(export_all).

add(X,Y) ->
	X + Y.

mul(X,Y) ->
	X * Y.

start_job_pool(Jobs) ->
	case whereis(job_pool) of
		undefined -> ok;
		Pool -> exit(Pool,ok)
	end,
	register(job_pool,spawn(simple,job_pool,[Jobs])),
	whereis(job_pool).

job_pool(Jobs) ->
	receive
		{get_job,Worker} ->
			case Jobs of
				[J|Js] ->
					io:format("~w->~w~n",[Worker,J]),
					Worker ! J,
					job_pool(Js);
				[] ->
					Worker ! no_jobs,
					job_pool(Jobs)
			end;
		{'EXIT',From,Reason} -> ok
	end.

worker(Master,Pool) ->
	io:format("~w@~w~n",[self(),node()]),
	Pool ! {get_job,self()},
	receive
		no_jobs -> ok;
		{Id,F} -> Master ! {Id,F()},
		worker(Master,Pool)
	end.

init_workers(Master,Pool,Nodes,NWorkers) ->
	lists:map(
		fun(Node) -> 
			[spawn(Node,
				fun() -> 
					worker(Master,Pool) 
				end) || _ <- lists:seq(1,NWorkers)] 
		end, Nodes).

start(Numbers) ->
	Master = self(),
	Nodes  = nodes(),

	Jobs = [{I,fun() -> add(X,Y) end} || {I,{X,Y}} <- lists:zip(lists:seq(1,length(Numbers)),Numbers)],
	Pool = start_job_pool(Jobs),
	init_workers(Master,Pool,Nodes,2),
	Res = [receive {JobId,R} -> R end || {JobId,_} <- Jobs].

test(N) ->
	Numbers = [{rand:uniform(200),rand:uniform(200)} || _ <- lists:seq(1,N)],
	lists:sum(start(Numbers)).