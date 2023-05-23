-module(bingo_worker).

-behaviour(gen_server).

-export([start_link/2]).

-export([ init/1
	, handle_call/3
	, handle_cast/2
	, handle_info/2
	, terminate/2
	, code_change/3
	, format_status/2
	]).

-import( bingo_tcp_conn
       , [ send/2
	 , send/3
	 ]
       ).

-include("bingo.hrl").

-record(state, {node, conn_pid}).

%%%===================================================================
%%% API Function
%%%===================================================================
start_link(Node, ConnPid) ->
    gen_server:start_link(?MODULE, [Node, ConnPid], []).

%%%===================================================================
%%% Callback Functions
%%%===================================================================
%%%=== init callback
init([Node, ConnPid]) ->
    process_flag(trap_exit, true),
    ok = bingo_mngr:add_node_worker(Node, self()),
    {ok, #state{node = Node, conn_pid = ConnPid}}.

%%%=== handle_call callback
%%%=== Send PING to target node / We are source node ================= 
handle_call( {ping, _Node} = Ping
	   , From
	   , #state{conn_pid = ConnPid} = State ) ->
    send(self(), ConnPid, {From, Ping}),
    {noreply, State};
%%%=== Send MULTI-PING to target node / We are source node ===========
handle_call( {multi_ping, From, Node}
	   , _
	   , #state{conn_pid = ConnPid} = State ) ->
    send(self(), ConnPid, {From, {multi_ping, Node}}),
    {reply, ok, State};
%%%=== Send CALL to target node / We are source node =================
handle_call( {call, _MFA} = Call
	   , From
	   , #state{conn_pid = ConnPid} = State ) ->
    send(self(), ConnPid, {From, Call}),
    {noreply, State};
%%%=== Send MULTI-CALL to target node / We are source node ===========
handle_call( {multi_call, From, MFA}
	   , _
	   , #state{conn_pid = ConnPid} = State ) ->
    send(self(), ConnPid, {From, {multi_call, MFA}}),
    {reply, ok, State};
%%%=== Send CAST to target node / We are source node =================
handle_call( {cast, _MFA} = Cast
	   , From
	   , #state{conn_pid = ConnPid} = State ) ->
    send(self(), ConnPid, {From, Cast}),
    {reply, ok, State};
%%%=== Send MULTI-CAST to target node / We are source node ===========
handle_call( {multi_cast, From, MFA}
	   , _
	   , #state{conn_pid = ConnPid} = State ) ->
    send(self(), ConnPid, {From, multi_cast, MFA}),
    {reply, ok, State};
%%%=== Unknown Request ===============================================
handle_call(_Request, _From, State) ->
    Reply = {error, unknown_request},
    {reply, Reply, State}.

%%%=== handle_cast callback 
%%%=== Receive PING from source node / We are target node ============
handle_cast( {From, {ping, TargetNode}}
	   , #state{conn_pid = ConnPid} = State ) ->
    Node = node(),
    Reply = case Node of
		TargetNode -> {pong, TargetNode};
		_Else -> {pang, TargetNode}
	    end,
    send(ConnPid, {From, Reply}),
    {noreply, State};
%%%=== Receive MULTI-PING from source node / We are target node ======
handle_cast( {From, {multi_ping, TargetNode}}
	   , #state{conn_pid = ConnPid} = State ) ->
    Node = node(),
    Reply = case Node of
		TargetNode -> {multi_pong, TargetNode};
		_Else -> {multi_pang, TargetNode}
	    end,
    send(ConnPid, {From, Reply}),
    {noreply, State};
%%%=== Receive PONG from target node / We are source node ============
handle_cast({From, {pong, _} = Reply}, #state{} = State) ->
    gen_server:reply(From, Reply),
    {noreply, State};
%%%=== Receive MULTI-PONG from target node / We are source node ======
handle_cast( {{FromPid, FromRef}, {multi_pong, Node}}
	   , #state{} = State ) ->
    gen_server:cast(FromPid, {pong, FromRef, Node}),
    {noreply, State};
%%%=== Receive PANG from Target Node / We are source node ============
handle_cast({From, {pang, _} = Reply}, #state{} = State) ->
    gen_server:reply(From, Reply),
    {noreply, State};
%%%=== Receive MULTI-PANG from target node / We are source node ======
handle_cast({{FromPid, FromRef}, {pang, Node}}, #state{} = State) ->
    gen_server:cast(FromPid, {pang, FromRef, Node}),
    {noreply, State};
%%%=== Receive CALL from source node / We are target node ============
handle_cast( {From, {call, {M, F, A}}}
	   , #state{node = Node, conn_pid = ConnPid} = State ) ->
    try
	send(ConnPid, {From, {call_reply, apply(M, F, A)}})
    catch 
	Class:Pattern -> 
	    send(ConnPid, {From, {call_reply, {Class, Pattern}}}),
            ?LOG_ERROR( "[BINGO] Exceptions,"
			" Class: ~p, Pattern: ~p @ {~p <---> ~p}"
                      , [Class, Pattern, Node, ConnPid] )
    end,
    {noreply, State};
%%%=== Receive MULTI-CALL from source node / We are target node =====
handle_cast( {From, {multi_call, {M, F, A}}}
	   , #state{node = Node, conn_pid = ConnPid} = State ) ->
    try
	send( ConnPid
	    , {From, {multi_call_reply, node(), apply(M, F, A)}}
	    )
    catch 
	Class:Pattern -> 
	    send( ConnPid
		, {From, {multi_call_reply, node(), {Class, Pattern}}}
		),
            ?LOG_ERROR( "[BINGO] Exceptions,"
			" Class: ~p, Pattern: ~p @ {~p <---> ~p}"
                      , [Class, Pattern, Node, ConnPid] )
    end,
    {noreply, State};
%%%=== Receive CALL-REPLY from target node / We are source node ======
handle_cast({From, {call_reply, Reply}}, #state{} = State) ->
    gen_server:reply(From, Reply),
    {noreply, State};
%%%=== Receive MULTI-CALL-REPLY from target node / We are source node 
handle_cast( {{FromPid, FromRef}, {multi_call_reply, Node, Reply}}
	   , #state{} = State ) ->
    gen_server:cast( FromPid
		   , {multi_call_reply, FromRef, Node, Reply}
		   ),
    {noreply, State};
%%%=== Receive CAST from source node / We are target node ============
handle_cast( {_From, {cast, {M, F, A}}}
	   , #state{node = Node, conn_pid = ConnPid} = State ) ->
    try 
	apply(M, F, A) 
    catch 
	Class:Pattern -> 
	    ?LOG_ERROR( "[BINGO] Exceptions,"
			" Class: ~p, Pattern: ~p @ {~p <---> ~p}"
		      , [Class, Pattern, Node, ConnPid] )
    end,
    {noreply, State};
%%%=== Receive MULTI-CAST from source node / We are target node =====
handle_cast( {_From, {multi_cast, {M, F, A}}}
	   , #state{node = Node, conn_pid = ConnPid} = State ) ->
    try
	apply(M, F, A)
    catch 
	Class:Pattern -> 
	    ?LOG_ERROR( "[BINGO] Exceptions,"
			" Class: ~p, Pattern: ~p @ {~p <---> ~p}"
                      , [Class, Pattern, Node, ConnPid] )
    end,
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%%%=== handle_info callback
handle_info(_Info, State) ->
    {noreply, State}.

%%%=== terminate callback
terminate(_Reason, #state{node = Node}) ->
    ok = bingo_mngr:del_node_worker(Node, self()),
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
