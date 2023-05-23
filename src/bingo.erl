-module(bingo).

-export([ echo/0
	, echo/1
	, ping/1
	, call/2
	, call/3
	, call/4
	, call/5
	, cast/2
	, cast/3
	, cast/4
	, cast/5
	, multi_ping/1
	, multi_call/2
	, multi_call/3
	, multi_call/4
	, multi_call/5
	, multi_cast/2
	, multi_cast/3
	, multi_cast/4
	, multi_cast/5
	, load/4
	, load_loop/3
	]).

-include("bingo.hrl").

%%%=== Test related functions ========================================
%%% These two functions are required for functionality tests.
echo() ->
    echo("echo/0").

echo(Term) ->
    Term.

%%%=== Diagnostics functions =========================================
-spec ping(node() | list(node())) -> 
	  {pong | pang, node()} | timeout | {error, invalid_params}.
ping(Node) when is_atom(Node) ->
    case bingo_mngr:get_node_worker(Node) of
	{ok, Worker} ->
	    gen_server:call(Worker, {ping, Node});
	Else ->
	    Else
    end;
ping(_) -> {error, invalid_params}.

multi_ping(Nodes) when is_list(Nodes) ->
    case bingo_mngr:get_mc_worker() of
	{ok, Worker} ->
	    gen_server:call(Worker, {multi_ping, Nodes});
	Else ->
	    Else
    end;
multi_ping(_) -> {error, invalid_params}.

%%%=== REMOTE-CALL functions =========================================
%%% Run `apply(erlang, F, [])` on target | remote node
%%% and waits forever to get reply.
-spec call(node(), atom()) -> 
	  ok | {error, invalid_params} | 
	  {error, not_available}.
call(Node, F) when is_atom(Node), is_atom(F) ->
    call(Node, erlang, F, [], infinity);
call(_, _) -> {error, invalid_params}.

%%% Run `apply(erlang, F, [])` on target | remote node
%%% and waits for T milliseconds to get reply.
-spec call(node(), atom(), integer() | atom()) ->
	  ok | {error, invalid_params} | timeout | 
	  {error, not_available}.
call(Node, F, T) when is_atom(Node), 
		      is_atom(F), 
		      is_integer(T) ->
    call(Node, erlang, F, [], T);
%%% Run `apply(M, F, [])` on target | remote node
%%% and waits forever to to get reply.
call(Node, M, F) when is_atom(Node),
		      is_atom(M),
		      is_atom(F) ->
    call(Node, M, F, [], infinity);
call(_, _, _) -> {error, invalid_params}.

%%% Run `apply(M, F, [])` on target | remote node
%%% and waits for T milliseconds to get reply.
-spec call(node(), atom(), atom(), integer() | list()) ->
	  {ok, map()} | {error, invalid_params} | timeout | 
	  {error, not_available}.
call(Node, M, F, T) when is_atom(Node),
			 is_atom(M),
			 is_atom(F),
			 is_integer(T) ->
    call(Node, M, F, [], T);
%%% Run `apply(M, F, A)` on target | remote node
%%% and waits forever to to get reply.
call(Node, M, F, A) when is_atom(Node),
			 is_atom(M),
			 is_atom(F),
			 is_list(A) ->
    call(Node, M, F, A, infinity);
call(_, _, _, _) -> {error, invalid_params}.

%%% Run `apply(M, F, A)` on target | remote node
%%% and waits for T milliseconds to get reply.
-spec call(node(), atom(), atom(), list(), integer()) -> 
	  {ok, map()} | {error, invalid_params} | timeout | 
	  {error, not_available}. 
call(Node, M, F, A, T) when is_atom(Node),
			    is_atom(M),
			    is_atom(F),
			    is_list(A), 
			    (is_integer(T) orelse T =:= infinity) ->
    case bingo_mngr:get_node_worker(Node) of
	{ok, Worker} ->
	    gen_server:call(Worker, {call, {M, F, A}}, T);
	Else ->
	    Else
    end;
call(_, _, _, _, _) -> {error, invalid_params}.

%%%=== REMOTE-CAST functions =========================================
%%% Run `apply(erlang, F, [])` on target | remote node
%%% and waits forever to gather node state.
-spec cast(node(), atom()) -> 
	  ok | {error, invalid_params} | {error, not_available}.
cast(Node, F) when is_atom(Node), is_atom(F) ->
    cast(Node, erlang, F, [], infinity);
cast(_, _) -> {error, invalid_params}.

%%% Run `apply(erlang, F, [])` on target | remote node
%%% and waits for T milliseconds to gather node state.
-spec cast(node(), atom(), integer() | atom()) ->
	  ok | {error, invalid_params} | timeout | 
	  {error, not_available}.
cast(Node, F, T) when is_atom(Node), 
		      is_atom(F), 
		      is_integer(T) ->
    cast(Node, erlang, F, [], T);
%%% Run `apply(M, F, [])` on target | remote node
%%% and waits forever to gather node state.
cast(Node, M, F) when is_atom(Node),
		      is_atom(M),
		      is_atom(F) ->
    cast(Node, M, F, [], infinity);
cast(_, _, _) -> {error, invalid_params}.

%%% Run `apply(M, F, [])` on target | remote node
%%% and waits for T milliseconds to gather node state.
-spec cast(node(), atom(), atom(), integer() | list()) ->
	  {ok, map()} | {error, invalid_params} | timeout | 
	  {error, not_available}.
cast(Node, M, F, T) when is_atom(Node),
			 is_atom(M),
			 is_atom(F),
			 is_integer(T) ->
    cast(Node, M, F, [], T);
%%% Run `apply(M, F, A)` on target | remote node
%%% and waits forever to gather node state.
cast(Node, M, F, A) when is_atom(Node),
			 is_atom(M),
			 is_atom(F),
			 is_list(A) ->
    cast(Node, M, F, A, infinity);
cast(_, _, _, _) -> {error, invalid_params}.

%%% Run `apply(M, F, A)` on target | remote node
%%% and waits for T milliseconds to gather node state.
-spec cast(node(), atom(), atom(), list(), integer()) -> 
	  {ok, map()} | {error, invalid_params} | timeout | 
	  {error, not_available}.
cast(Node, M, F, A, T) when is_atom(Node),
			    is_atom(M),
			    is_atom(F),
			    is_list(A), 
			    (is_integer(T) orelse T =:= infinity) ->
    case bingo_mngr:get_node_worker(Node) of
	{ok, Worker} ->
	    gen_server:call(Worker, {cast, {M, F, A}}, T);
	Else ->
	    Else
    end;
cast(_, _, _, _, _) -> {error, invalid_params}.

%%%=== REMOTE-MULTI-CALL functions ===================================
%%% These functions return results in a map. In this map, the key is 
%%% the node name and the value of one key can have three states: 
%%% `reply`, `waiting`, or `not_available`. 
%%% If one node is not available in the cluster, 
%%% its state is `not_available`. 
%%% If one node is available, but does not deliver its reply within 
%%% the timeout period, its state is `waiting`. Otherwise, 
%%% the state contains the reply.

%%% Run `apply(erlang, F, [])` on target | remote nodes
%%% and gather replies.
-spec multi_call(list(node()), atom()) -> 
	  {ok, map()} | {error, invalid_params}.
multi_call(Nodes, F) when is_list(Nodes), is_atom(F) ->
    multi_call(Nodes, erlang, F, [], infinity);
multi_call(_, _) -> {error, invalid_params}.

%%% Run `apply(M, F, [])` on target | remote nodes
%%% and waits for T milliseconds to gather replies. 
-spec multi_call(list(node()), atom(), integer() | atom()) ->
	  {ok, map()} | {error, invalid_params} | timeout.
multi_call(Nodes, F, T) when is_list(Nodes), 
			     is_atom(F), 
			     is_integer(T) ->
    multi_call(Nodes, erlang, F, [], T);
%%% Run `apply(M, F, [])` on target | remote nodes
%%% and waits forever to gather replies. 
multi_call(Nodes, M, F) when is_list(Nodes),
			     is_atom(M),
			     is_atom(F) ->
    multi_call(Nodes, M, F, [], infinity);
multi_call(_, _, _) -> {error, invalid_params}.

%%% Run `apply(M, F, [])` on target | remote nodes
%%% and waits for T milliseconds to gather replies. 
-spec multi_call( list(node())
		, atom()
		, atom()
		, integer() | list() ) ->
	  {ok, map()} | {error, invalid_params} | timeout.
multi_call(Nodes, M, F, T) when is_list(Nodes),
				is_atom(M),
				is_atom(F),
				is_integer(T) ->
    multi_call(Nodes, M, F, [], T);
%%% Run `apply(M, F, A)` on target | remote nodes
%%% and waits forever to gather replies. 
multi_call(Nodes, M, F, A) when is_list(Nodes),
				is_atom(M),
				is_atom(F),
				is_list(A) ->
    multi_call(Nodes, M, F, A, infinity);
multi_call(_, _, _, _) -> {error, invalid_params}.

%%% Run `apply(M, F, A)` on target | remote nodes
%%% and waits for T milliseconds to gather replies. 
-spec multi_call(list(node()), atom(), atom(), list(), integer()) -> 
	  {ok, map()} | {error, invalid_params} | timeout.
multi_call(Nodes, M, F, A, T) when is_list(Nodes), is_atom(M),
				   is_atom(F), is_list(A), 
				   ( is_integer(T) orelse 
				     T =:= infinity ) ->
    case bingo_mngr:get_mc_worker() of
	{ok, Worker} ->
	    gen_server:call( Worker
			   , {multi_call, Nodes, {M, F, A}}
			   , T
			   );
	Else ->
	    Else
    end;
multi_call(_, _, _, _, _) -> {error, invalid_params}.

%%%=== REMOTE-MULTI-CAST functions ===================================
%%% These functions return results in a map. In this map, the key 
%%% is the node name and the value of one key can have three states: 
%%% `ok`, `waiting`, or `not_available`. If one node is not available 
%%% in the cluster, its state is `not_available`. If one node is 
%%% available, but does not deliver its `ok` within the timeout period, 
%%% its state is `waiting`. Otherwise, the state contains the `ok`.

%%% Run `apply(erlang, F, [])` on target | remote nodes
%%% and waits forever to gather node states.
-spec multi_cast(list(node()), atom()) -> 
	  {ok, map()} | {error, invalid_params}.
multi_cast(Nodes, F) when is_list(Nodes), is_atom(F) ->
    multi_cast(Nodes, erlang, F, [], infinity);
multi_cast(_, _) -> {error, invalid_params}.

%%% Run `apply(erlang, F, [])` on target | remote nodes
%%% and waits for T milliseconds to gather node states.
-spec multi_cast(list(node()), atom(), integer() | atom()) ->
	  {ok, map()} | {error, invalid_params} | timeout.
multi_cast(Nodes, F, T) when is_list(Nodes), 
			     is_atom(F), 
			     is_integer(T) ->
    multi_cast(Nodes, erlang, F, [], T);
%%% Run `apply(M, F, [])` on target | remote nodes
%%% and waits forever to gather node states.
multi_cast(Nodes, M, F) when is_list(Nodes),
			     is_atom(M),
			     is_atom(F) ->
    multi_cast(Nodes, M, F, [], infinity);
multi_cast(_, _, _) -> {error, invalid_params}.

%%% Run `apply(M, F, [])` on target | remote nodes
%%% and waits for T milliseconds to gather node states.
-spec multi_cast(list(node()), atom(), atom(), integer() | list()) ->
	  {ok, map()} | {error, invalid_params} | timeout.
multi_cast(Nodes, M, F, T) when is_list(Nodes),
				is_atom(M),
				is_atom(F),
				is_integer(T) ->
    multi_cast(Nodes, M, F, [], T);
%%% Run `apply(M, F, A)` on target | remote nodes
%%% and waits forever to gather node states.
multi_cast(Nodes, M, F, A) when is_list(Nodes),
				is_atom(M),
				is_atom(F),
				is_list(A) ->
    multi_cast(Nodes, M, F, A, infinity);
multi_cast(_, _, _, _) -> {error, invalid_params}.

%%% Run `apply(M, F, A)` on target | remote nodes
%%% and waits for T milliseconds to gather node states.
-spec multi_cast(list(node()), atom(), atom(), list(), integer()) -> 
	  {ok, map()} | {error, invalid_params} | timeout.
multi_cast(Nodes, M, F, A, T) when is_list(Nodes), is_atom(M),
				   is_atom(F), is_list(A), 
				   ( is_integer(T) orelse 
				     T =:= infinity ) ->
    case bingo_mngr:get_mc_worker() of
	{ok, Worker} ->
	    gen_server:call( Worker
			   , {multi_cast, Nodes, {M, F, A}}
			   , T
			   );
	Else ->
	    Else
    end;
multi_cast(_, _, _, _, _) -> {error, invalid_params}.

%%% Load Generators / Only for test!
load(Type, Nodes, Concurrency, Count) ->
    make_node_conn(Nodes),
    timer:sleep(400),
    [ begin
	  Pid = spawn(?MODULE, load_loop, [Type, Nodes, Count]), 
	  ?LOG_DEBUG("~p: Start", [Pid])
      end || _ <- lists:seq(1, Concurrency) ],
    ok.

load_loop(_Type, _Node, 0) -> ?LOG_DEBUG("~p: Stop", [self()]);
load_loop(Type, Node, Count) when is_atom(Node) ->
    MaxBytes = 100000*8,
    MaxDummyNum = 1000000,
    RandomMessage = << (rand:uniform(MaxDummyNum)):
		       (rand:uniform(MaxBytes) )>>,
    case Type of
	call ->
	    RandomMessage = bingo:call( Node
				      , bingo
				      , echo
				      , [RandomMessage] );
	cast ->
	    ok = bingo:cast(Node, bingo, echo, [RandomMessage])
    end,
    load_loop(Type, Node, Count - 1);
load_loop(Type, Nodes, Count) when is_list(Nodes) ->
    MaxBytes = 100000*8,
    MaxDummyNum = 1000000,
    RandomMessage = << (rand:uniform(MaxDummyNum)):
		       (rand:uniform(MaxBytes)) >>,
    case Type of
	multi_call ->
	    bingo:multi_call(Nodes, bingo, echo, [RandomMessage]);
	multi_cast ->
	    bingo:multi_cast(Nodes, bingo, echo, [RandomMessage])
    end,
    load_loop(Type, Nodes, Count - 1). 

make_node_conn([]) -> ok;
make_node_conn(Node) when is_atom(Node) ->
    bingo_mngr:get_node_worker(Node);
make_node_conn([Node | R]) ->
    make_node_conn(Node),
    make_node_conn(R).


    

    
    
