%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This implements a page rank algorithm using map-reduce
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(page_rank).
-compile(export_all).

%% Use map_reduce to count word occurrences

map(Url,ok) ->
    [{Url,Body}] = dets:lookup(web,Url),
    Urls = crawl:find_urls(Url,Body),
    [{U,1} || U <- Urls].

reduce(Url,Ns) ->
    [{Url,lists:sum(Ns)}].

page_rank() ->
    {ok,web} = dets:open_file(web,[{file,"web.dat"}]),
    Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
    map_reduce:map_reduce_seq(fun map/2, fun reduce/2, 
			      [{Url,ok} || Url <- Urls]).

page_rank_par() ->
    dets:open_file(web,[{file,"web.dat"}]),
    Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
    map_reduce:map_reduce_par(fun map/2, 32, fun reduce/2, 32, 
			      [{Url,ok} || Url <- Urls]).

% page_rank_par but distributed on available nodes
page_rank_par_dist() ->
    dets:open_file(web,[{file,"web.dat"}]),
    Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
    map_reduce:map_reduce_par_dist(fun map/2, 32, fun reduce/2, 32, 
			      [{Url,ok} || Url <- Urls]).

%uses a jobpool, with workers on diffrent nodes able to take work from the pool
page_rank_par_dist_load() ->
    dets:open_file(web,[{file,"web.dat"}]),
    Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
    map_reduce:map_reduce_par_dist_load(fun map/2, 32, fun reduce/2, 32, 
                      [{Url,ok} || Url <- Urls]).

% Below are functions for benchmarking the different versions
-define(EXECUTIONS,10).

repeat(F) ->
    [F() || _ <- lists:seq(1,?EXECUTIONS)].

bm(F) ->
    {T,_} = timer:tc(?MODULE,repeat,[F]),
    T/?EXECUTIONS/1000.

benchmark_seq() ->
    bm(fun page_rank/0)/1000.

benchmark_par() ->
    bm(fun page_rank_par/0)/1000.

benchmark_par_dist() -> 
    bm(fun page_rank_par_dist/0)/1000.

benchmark_par_dist_load() -> 
    bm(fun page_rank_par_dist_load/0)/1000.