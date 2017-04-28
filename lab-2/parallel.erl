-module(parallel).
-compile(export_all).

% Parallel map that cares about order of receives
map(Fun, L) -> 
  Parent = self(),
  Pids = lists:map(fun(X) -> 
    spawn_link(fun() -> 
      case catch Fun(X) of
        {'EXIT',_} -> 
          Parent ! {self(), {'EXIT',no_solution}};
        Res ->
          Parent ! {self(), Res }
      end
    end)
  end, L),
  [receive 
      {Pid,Res} -> Res 
    end || Pid <- Pids]. 


newLog(Data) ->
  io:format("Value is : ~p",[Data]),
  io:nl().
