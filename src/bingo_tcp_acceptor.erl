-module(bingo_tcp_acceptor).

-behaviour(supervisor).

-export([ start_link/2
	, start_acceptor/2
	]).

-export([init/1]).

-include("bingo.hrl").

%% === @TODO: Add specs.

%%====================================================================
%% API functions
%%====================================================================
start_link(Socket, Acceptors) ->
    SupId = list_to_atom("bingo_acceptors_sup"),
    {ok, ASup} = supervisor:start_link( {local, SupId}
				      , ?MODULE
				      , [Socket, Acceptors]
				      ),
    {ok, ASup}.

start_acceptor(AId, Socket) ->
    F = fun() -> acceptor_loop(Socket) end,
    Pid = spawn_link(F),
    register(AId, Pid),
    {ok, Pid}.

%%====================================================================
%% Callback Functions
%%====================================================================
init([Socket, Acceptors]) ->
    SupFlags = #{ strategy => one_for_one
		, intensity => 10
		, period => 1
		},
    AcceptorSpecs = [ begin
			  Pref = "bingo_acceptor_",
			  Id = Pref ++ integer_to_list(Index),
			  AName = list_to_atom(Id),
			  #{ id => make_ref()
			   , start => { ?MODULE
				      , start_acceptor
				      , [AName, Socket]
				      }
			   , shutdown => 3000
			   , type => worker
			   , modules => [?MODULE] 
			   } 
		      end || Index <- lists:seq(1, Acceptors)
		    ],
    {ok, {SupFlags, AcceptorSpecs}}.

%%====================================================================
%% Private Functions
%%====================================================================
acceptor_loop(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    {ok, ConnPID} = bingo_tcp_conn:start(Socket),
    gen_tcp:controlling_process(Socket, ConnPID),
    acceptor_loop(LSocket).

