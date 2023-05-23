-module(bingo_tcp_conn).

-behaviour(gen_server).

-export([ start/1
	, start/4
	, send/2
	, send/3
	]).

-export([ init/1
	, handle_call/3
	, handle_cast/2
	, handle_info/2
	, terminate/2
	, code_change/3
	, format_status/2
	]).

-define(SERVER, ?MODULE).
-define(E(T), term_to_binary(T)). % E: Encode
-define(D(B), binary_to_term(B)). % D: Decode
-define(S(Sock, T), gen_tcp:send(Sock, ?E(T))).
-define(R, make_ref()).

-include("bingo.hrl").

-record(state, {ip, port, node, workers, socket, refs}).

%%%===================================================================
%%% API Functions
%%%===================================================================
start(Ip, Port, Node, Workers) ->
    {ok, Conn} = 
	gen_server:start( ?MODULE
			, [Ip, Port, Node, Workers]
			, []),
    gen_server:call(Conn, get_workers).

start(Socket) ->
    gen_server:start(?MODULE, [Socket], []).

%%%=== Send CAST on TCP Connection =================================== 
send(ConnPid, Term) ->
    gen_server:cast(ConnPid, {send, Term}).
%%%=== Send CALL on TCP Connection / Worker requires a reply =========
send(WorkerPid, ConnPid, Term) ->
    gen_server:cast(ConnPid, {send, WorkerPid, Term}).
    
%%%===================================================================
%%% Callback Functions
%%%===================================================================
%%%=== init callback
%%%=== Init As TCP-CLIIENT / But finally, we require a socket ========
init([Ip, Port, Node, Workers]) ->
    process_flag(trap_exit, true),
    %% === As Client
    {ok, Socket} = gen_tcp:connect(Ip, Port, ?BINGO_TCP_DEFAULT_OPTS),
    ok = ?S(Socket, {add_node, node()}),
    {ok, WorkerPids} = start_workers(Node, Workers),
    ?LOG_DEBUG( "[BINGO] New Connection: ~p <---> ~p, with ~p Workers."
	      , [Node, self(), Workers] ),
    {ok, #state{ ip = Ip 
	       , port = Port
	       , node = Node 
	       , socket = Socket
	       , workers = {Workers, WorkerPids}
	       , refs = #{}
	       }};
%%%=== Init As TCP-SERVER / Workers will run by client message! ======
init([Socket]) ->
    process_flag(trap_exit, true),
    %% === As Server
    {ok, #state{ socket = Socket
	       , refs = #{}
	       }}.

%%%=== handle_call callback
%%%===  Return Workers Info ==========================================
handle_call(get_workers, _From, #state{workers = Workers} = State) ->
    Reply = {ok, Workers},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = {error, unknown_request},
    {reply, Reply, State}.

%%%=== handle_cast callback
%%%===  Send CAST frame on TCP connection ============================  
handle_cast({send, Frame}, #state{socket = Socket} = State) ->
    ok = ?S(Socket, Frame),
    {noreply, State};
%%%===  Send CALL frame on TCP connection ============================
handle_cast( {send, WorkerPid, {From, _Term} = Frame}
	   , #state{socket = Socket, refs = Refs} = State ) ->
    ok = ?S(Socket, Frame),
    NewState = State#state{refs = Refs#{From => WorkerPid}},
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

%%%=== handle_info callback
%%%===  If a worker killed, start that worker again ==================
handle_info( {'EXIT', FromPid, _Reason}
	   , #state{workers = {Workers, OldWorkers}} = State ) ->
    {_, NewWorkers} = 
	maps:fold( fun(I, FP, {FP, ACC}) ->
			   {ok, NWP} = 
			       bingo_worker:start_link(self()),
			   {FP, ACC#{I => NWP}};
		      (I, WP, {FP, ACC}) ->
			   {FP, ACC#{I => WP}}
		   end
		 , {FromPid, #{}}, OldWorkers),
    NewState = State#state{workers = {Workers, NewWorkers}},
    {noreply, NewState}; 
%%%=== Stop the TCP connection holder ================================
handle_info( {tcp_error, Socket, _Reason}
	   , #state{socket = Socket} = Conn ) ->
    {stop, normal, Conn};
%%%=== Stop the TCP connection holder ================================
handle_info( {tcp_closed, Socket}
	   , #state{socket = Socket} = Conn ) ->
    {stop, normal, Conn};
%%%=== Handle frames that receive from TCP connection ================
handle_info({tcp, _, FrameBin}, #state{socket = Socket} = State) ->
    NewState = case ?D(FrameBin) of
		   {add_node, Node} ->
		       %% We are TCP-SERVER: so we start our workers 
		       %% after the client's workers.
		       {ok, #{workers := WCnt}} = 
			   bingo_conf:get(conn),
		       {ok, WorkerPids} = start_workers(Node, WCnt),
		       ?LOG_DEBUG( "[BINGO] New Connection:"
				   " ~p <---> ~p, with ~p Workers."
				 , [Node, self(), WCnt] ),
		       State#state{ node = Node
				  , workers = {WCnt, WorkerPids} };
		   Frame ->
		       {ok, NewState_} = dispatch(Frame, State),
		       NewState_
	       end,
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

%%%=== terminate callback
terminate(_Reason, #state{node = Node, workers = {WCnt, _}}) ->
    ?LOG_DEBUG( "[BINGO] Killed Connection:"
		" ~p <---> ~p, with ~p Workers."
	      , [Node, self(), WCnt]
	      ),
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
start_workers(Node, Workers) ->
    { ok
    , lists:foldl(fun(Index, ACC) -> 
			  {ok, Pid} = 
			      bingo_worker:start_link(Node, self()),
			  ACC#{Index => Pid}
		  end, #{}, lists:seq(1, Workers))
    }.

dispatch( {From, _} = Frame
	, #state{workers = Workers, refs = Refs} = State ) ->
    { ok
    , case maps:take(From, Refs) of
	  error ->
	      %% This is a new Frame! 
	      %% that no worker doesn't want it!
	      %% so we select a random worker and 
	      %% send the frame to it.
	      gen_server:cast(get_worker(Workers), Frame),
	      State;
	  {WorkerPid, NewRefs} -> 
	      %% This is a new Frame!
	      %% But a worker wants it!
	      %% so we found that worker and 
	      %% send the frame to it.
	      gen_server:cast(WorkerPid, Frame),
	      State#state{refs = NewRefs}
      end
    }.
    
get_worker({WorkerCnt, WorkerPids}) ->
    maps:get(rand:uniform(WorkerCnt), WorkerPids).
	
