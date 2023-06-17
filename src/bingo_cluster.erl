
-module(bingo_cluster).

-export([ make_node_conn/1
	, get_conn_info/0
	]).

-include("bingo.hrl").

%%====================================================================
%% Public Function: make_node_conn/1
%%====================================================================
-spec make_node_conn(node()) -> 
	  {ok, {integer(), map()}} | {error, not_available}.
make_node_conn(TargetNode) ->
    {ok, CTO} = bingo_conf:get('cluster.erpc.timeout'),
    %% First, get the target node and its configuration 
    %% from the out-of-the-band channel...
    case rpc:call( TargetNode
		 , bingo_cluster
		 , get_conn_info
		 , []
		 , CTO) of
	{ok, #bingo_conn_info{ node = TargetNode
			     , ip = Ip
			     , port = Port
			     , conns = Conns
			     , workers = Workers
			     }} ->
	    %% ...And then make a new channel 
	    %% between this and the target node.
	    WorkerInfoMaker = fun(_, WP, {WC, WPs}) ->
				      I = WC + 1,
				      {I, WPs#{I => WP}}
			      end, 
	    ConnectionMaker = 
		fun(_I0, {WCnt, WPids}) ->
			{ok, {_WorkerCnt, WorkerPids}} =  
			    bingo_tcp_conn:start( Ip
						, Port
						, TargetNode
						, Workers
						),
			maps:fold( WorkerInfoMaker 
				 , {WCnt, WPids} 
				 , WorkerPids)
		end,
	    { ok
	    , lists:foldl( ConnectionMaker 
			 , {0, #{}}
			 , lists:seq(1, Conns))
	    };	    
	%% ...Or if the target node is not present 
	%% in the out-of-the-band channel 
	%% generate the proper error!
	_Else ->
	    {error, not_available}
    end.

%%====================================================================
%% Public Function: get_conn_info/1
%%====================================================================
get_conn_info() ->
    {ok, #{ ip := Ip0
	  , port := Port
	  , conns := ConnCnt
	  , workers := WorkerCnt
	  }} = bingo_conf:get(conn),
    {ok, Ip} = inet:parse_ipv4_address(Ip0),
    {ok, #bingo_conn_info{ node = node()
			 , ip = Ip
			 , port = Port
			 , conns = ConnCnt
			 , workers = WorkerCnt
			 }}.
