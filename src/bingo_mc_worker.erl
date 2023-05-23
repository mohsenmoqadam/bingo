-module(bingo_mc_worker).

-behaviour(gen_server).

-export([start_link/1]).

-export([ init/1
	, handle_call/3
	, handle_cast/2
	, handle_info/2
	, terminate/2
	, code_change/3
	, format_status/2
	]).

-include("bingo.hrl").

-record(state, {refs}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    bingo_mngr:add_mc_worker(self()),
    {ok, #state{refs = #{}}}.
handle_call({multi_ping, Nodes}, From, #state{refs = Refs} = State) ->
    Ref = make_ref(),
    Pred = fun(Node, {AvailableNodesCnt_, NodesReply_}) -> 
		   case bingo_mngr:get_node_worker(Node) of
		       {error, not_available} ->
			   { AvailableNodesCnt_
			   , NodesReply_#{Node => not_available}
			   };
		       {ok, Worker} ->
			   gen_server:call( Worker
					  , { multi_ping
					    , {self(), Ref}
					    , Node
					    }
					  ),
			   { AvailableNodesCnt_ + 1
			   , NodesReply_#{Node => timeout}
			   }
		   end
	   end,
    {AvailableNodesCnt, NodesReply} = lists:foldl( Pred
						 , {0, #{}}
						 , Nodes ),
    if AvailableNodesCnt =:= 0 -> 
	    {reply, {ok, NodesReply}, State};
       true ->	    
	    {ok, MCTO} = bingo_conf:get('mc.timeout'), 
	    TR = erlang:send_after(MCTO, self(), {mc_timeout, Ref}),
	    NewRefs = Refs#{ Ref => { From
				    , AvailableNodesCnt
				    , NodesReply
				    , TR
				    } },
	    NewState = State#state{refs = NewRefs},
	    {noreply, NewState}
    end;
handle_call( {multi_call, Nodes, MFA}
	   , From
	   , #state{refs = Refs} = State ) ->
    Ref = make_ref(),
    Pred = fun(Node, {AvailableNodesCnt_, NodesReply_}) -> 
		   case bingo_mngr:get_node_worker(Node) of
		       {error, not_available} ->
			   { AvailableNodesCnt_
			   , NodesReply_#{Node => not_available}
			   };
		       {ok, Worker} ->
			   gen_server:call( Worker
					  , { multi_call
					    , {self(), Ref}
					    , MFA
					    }
					  ),
			   { AvailableNodesCnt_ + 1
			   , NodesReply_#{Node => timeout} }
		   end
	   end, 
    {AvailableNodesCnt, NodesReply} = 
	lists:foldl( Pred
		   , {0, #{}}
		   , Nodes),
    if AvailableNodesCnt =:= 0 -> 
	    {reply, {ok, NodesReply}, State};
       true ->	    
	    {ok, MCTO} = bingo_conf:get('mc.timeout'), 
	    TR = erlang:send_after(MCTO, self(), {mc_timeout, Ref}),
	    NewRefs = Refs#{ Ref => { From
				    , AvailableNodesCnt
				    , NodesReply
				    , TR
				    }
			   },
	    NewState = State#state{refs = NewRefs},
	    {noreply, NewState}
    end;
handle_call({multi_cast, Nodes, MFA}, _From, #state{} = State) ->
    Ref = make_ref(),
    Pred = fun(Node, {AvailableNodesCnt_, NodesReply_}) -> 
		   case bingo_mngr:get_node_worker(Node) of
		       {error, not_available} ->
			   { AvailableNodesCnt_
			   , NodesReply_#{Node => not_available}
			   };
		       {ok, Worker} ->
			   gen_server:call( Worker
					  , { multi_cast
					    , {self(), Ref}
					    , MFA
					    }
					  ),
			   { AvailableNodesCnt_ + 1
			   , NodesReply_#{Node => ok} 
			   }
		   end
	   end,
    {_AvailableNodesCnt, NodesReply} = 
	lists:foldl( Pred
		   , {0, #{}}
		   , Nodes),
    {reply, {ok, NodesReply}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast( {Reply, Ref, Node}
	   , #state{refs = Refs} = State ) when Reply =:= pong;
						Reply =:= pang ->
    case maps:take(Ref, Refs) of
	error -> {noreply, State};
	{{From, Remained, NodeState, TR}, NewRefs} ->
	    {NewRemained, NewNodeState} = 
		case maps:take(Node, NodeState) of
		    error -> 
			{Remained, NodeState};
		    {_, NodeState_} -> 
			{ Remained - 1
			, NodeState_#{Node => Reply}
			} 
		end,
	    if NewRemained =:= 0 -> 
		    erlang:cancel_timer(TR),
		    gen_server:reply(From, {ok, NewNodeState}),
		    {noreply, State#state{refs = NewRefs}};
	       true ->
		    { noreply
		    , State#state{ refs = NewRefs#{ Ref => { From
							   , NewRemained
							   , NewNodeState
							   , TR
							   }
						  }
				 }

		    }
	    end
    end;
handle_cast( {multi_call_reply, Ref, Node, Reply}
	   , #state{refs = Refs} = State ) ->
    case maps:take(Ref, Refs) of
	error -> {noreply, State};
	{{From, Remained, NodeState, TR}, NewRefs} ->
	    {NewRemained, NewNodeState} = 
		case maps:take(Node, NodeState) of
		    error -> 
			{Remained, NodeState};
		    {_, NodeState_} -> 
			{ Remained - 1 
			, NodeState_#{Node => Reply}
			} 
		end,
	    if NewRemained =:= 0 -> 
		    erlang:cancel_timer(TR),
		    gen_server:reply(From, {ok, NewNodeState}),
		    {noreply, State#state{refs = NewRefs}};
	       true ->
		    { noreply
		    , State#state{ refs = NewRefs#{Ref => { From
							  , NewRemained
							  , NewNodeState
							  , TR
							  }
						  }
				 }
		    }
	    end
    end;
handle_cast(_Request, State) ->
    {noreply, State}.
 
handle_info({mc_timeout, Ref}, #state{refs = Refs} = State) ->
    case maps:take(Ref, Refs) of
	error -> 
	    {noreply, State};
	{{From, _Remained, NodeState, TR}, NewRefs} ->
	    erlang:cancel_timer(TR),
	    gen_server:reply(From, {ok, NodeState}),
	    {noreply, State#state{refs = NewRefs}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    bingo_mngr:del_mc_worker(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
