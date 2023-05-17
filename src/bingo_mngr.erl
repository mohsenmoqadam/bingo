-module(bingo_mngr).

-behaviour(gen_server).

-export([ start_link/0
	, add_node_worker/2
	, del_node_worker/2
	, get_node_worker/1
	, show_node_workers/1
	, add_mc_worker/1
	, del_mc_worker/1
	, get_mc_worker/0
	, show_mc_workers/0
	]).

-export([ init/1
	, handle_call/3
	, handle_cast/2
	, handle_info/2
	, terminate/2
	, code_change/3
	, format_status/2
	]).

-include("bingo.hrl").

-define(SERVER, ?MODULE).
-define(NODES, bingo_nodes).
-define(MCW, bingo_mc_workers).
-record(state, {}).

%%%===================================================================
%%% API Functions
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%=== Worker management functions ===================================
%%% We hold nodes information in the application ETS table `ac_tab`, 
%%% so for preventing race-condition only serialize write-dependent 
%%% requests on this table and read-oriented requests serve without 
%%% serialization. 
add_node_worker(Node, Worker) ->
    gen_server:cast(?SERVER, {add_node_worker, Node, Worker}).

del_node_worker(Node, Worker) ->
    gen_server:cast(?SERVER, {del_node_worker, Node, Worker}).

%%% If the node does not exist or not has any worker, 
%%% first this function creates connections and workers for each 
%%% connection and then returns the pid of one created worker.
get_node_worker(Node)->
    get(?NODES, Node).

%%% This function only reports workers of the node and can't create 
%% connections to the node and workers of each connection.
show_node_workers(Node) ->
    show_all(?NODES, Node).

%%% We hold multi-call workers information in the 
%%% application ETS table `ac_tab`, so for preventing race-condition 
%%% only serialize write-dependent requests on this table and 
%%% read-oriented requests serve without serialization. 
add_mc_worker(Worker) ->
    gen_server:cast(?SERVER, {add_mc_worker, Worker}).

del_mc_worker(Worker) ->
    gen_server:cast(?SERVER, {del_mc_worker, Worker}).

%%% Selects one worker randomly and returns its pid.
get_mc_worker()->
    get(?MCW, node()).

%%% show pid of current multi-call workers.
show_mc_workers() ->
    show_all(?MCW, node()).

%%%===================================================================
%%% Callback Functions
%%%===================================================================
%%%=== init callback
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%%=== init callback
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%%=== handle_cast callback
handle_cast({add_node_worker, Node, Worker}, State) ->
    add(?NODES, Node, Worker),
    {noreply, State};
handle_cast({del_node_worker, Node, Worker}, State) ->
    del(?NODES, Node, Worker),
    {noreply, State};
handle_cast({add_mc_worker, Worker}, State) ->
    add(?MCW, node(), Worker),
    {noreply, State};
handle_cast({del_mc_worker, Worker}, State) ->
    del(?MCW, node(), Worker),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%%=== handle_info callback
handle_info(_Info, State) ->
    {noreply, State}.

%%%=== terminate callback
terminate(_Reason, _State) ->
    ok.

%%%=== code_change callback
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=== format_status callback
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Private functions
%%%===================================================================
%%% We have a configuration metric with the name 'bingo_nodes', 
%%% and map value. The key in this map is the node name and the value 
%%% is a tuple. the first element of this tuple holds the number 
%%% of workers and the second element holds one another map. 
%%% Keys in this map are one continuous and incremental index that 
%%% starts from one and the value is the Pid of a worker.
%%%
%%% With above comment bingo_nodes is: 
%%% #{node_name => {number_of_workers, #{worker_index => worker_pid}}}
%%% This structure helps us when requiring a worker, we get one of 
%%% them rapidly and randomly with the lowest cost, Like:
%%% WorkerPid = maps:get(rand:uniform(NumberOfWorkers), WorkerPids),
%%% when NumberOfWorkers is number_of_workers and
%%% WorkerPids is #{worker_index => worker_pid}.
%%%
%%% All above comment is valid for multi-call worker!
%%%
%%% Below function manage the above-commented data structure:
add(CN, Node, Worker) ->
    OldNodes = 
	case bingo_conf:get(CN) of
	    {error, undefined} ->
		#{};
	    {ok, Nodes} ->
		Nodes
	end,
    NewNodes = 
	case maps:take(Node, OldNodes) of
	    error -> OldNodes#{Node => {1, #{1 => Worker}}};
	    {{_, NodeWorkerPids0}, Nodes0} ->
		{_, NodeWorkerCnt, NodeWorkerPids} = 
		    maps:fold(fun(_K, W, {W, NWC, NWP}) ->
				      {W, NWC, NWP};
				 (_K, V, {W, NWC, NWP}) ->
				      {W, NWC + 1, NWP#{NWC + 1 => V}}
			      end
			     , {Worker, 0, #{}}
			     , NodeWorkerPids0),
		Index = NodeWorkerCnt + 1,
		Nodes0#{Node => {Index, NodeWorkerPids#{Index => Worker}}}
	end,
    ok = bingo_conf:set(CN, NewNodes),
    ok.

del(CN, Node, Worker) ->
    OldNodes = 
	case bingo_conf:get(CN) of
	    {error, undefined} ->
		#{};
	    {ok, Nodes} ->
		Nodes
	end,
    NewNodes = 
	case maps:take(Node, OldNodes) of
	    error -> OldNodes#{Node => {0, #{}}};
	    {{_, NodeWorkerPids0}, Nodes0} ->
		{_, NodeWorkerCnt, NodeWorkerPids} = 
		    maps:fold(fun(_K, W, {W, NWC, NWP}) ->
				      {W, NWC, NWP};
				 (_K, V, {W, NWC, NWP}) ->
				      {W, NWC + 1, NWP#{NWC + 1 => V}}
			      end
			     , {Worker, 0, #{}}
			     , NodeWorkerPids0),
		Nodes0#{Node => {NodeWorkerCnt, NodeWorkerPids}}
	end,
    ok = bingo_conf:set(CN, NewNodes),
    ok.

get(CN, Node) ->
    case bingo_conf:get(CN) of
	{error, undefined} ->
	    case bingo_cluster:make_node_conn(Node) of
		{ok, {WorkerCnt, WorkerPids}} ->
		    ok = bingo_conf:set(?NODES, #{Node => {WorkerCnt, WorkerPids}}),
		    Worker = maps:get(rand:uniform(WorkerCnt), WorkerPids),
		    {ok, Worker};
		Else ->
		    Else
	    end;
	{ok, Nodes} ->
	    case maps:get(Node, Nodes, {0, #{}}) of
		{0, _} ->
		    case bingo_cluster:make_node_conn(Node) of
			{ok, {WorkerCnt, WorkerPids}} ->
			    ok = bingo_conf:set(CN, Nodes#{Node => {WorkerCnt, WorkerPids}}),
			    Worker = maps:get(rand:uniform(WorkerCnt), WorkerPids),
			    {ok, Worker};
			Else ->
			    Else
		    end;
		{WorkerCnt, WorkerPids} ->
		    Worker = maps:get(rand:uniform(WorkerCnt), WorkerPids),
		    {ok, Worker}
	    end
    end.

show_all(CN, Node) ->
    case bingo_conf:get(CN) of
    	{error, undefined} ->
	    {error, not_available};
     	{ok, Nodes} ->
	    case maps:get(Node, Nodes, not_available) of
     		not_available ->
		    {error, not_available};
     		{WorkerCnt, WorkerPids} ->
		    {ok, WorkerCnt, WorkerPids}
     	    end
    end.
 
