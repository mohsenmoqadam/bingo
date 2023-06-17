-module(bingo_tcp_listener).

-behaviour(gen_server).

-export([start_link/0]).

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

-record(state, {socket}).

%% === @TODO: Add specs.

%%%===================================================================
%%% API Functions
%%%===================================================================
start_link() ->
    Id = list_to_atom("bingo_listener"),
    gen_server:start_link({local, Id}, ?MODULE, [], []).

%%%===================================================================
%%% Callback Functions
%%%===================================================================
init([]) ->
    Conn = case bingo_conf:get(conn) of
	       {ok, #{port := {PortStart, PortEnd}} = Conn0} ->
		   RP = rand:uniform(PortEnd - PortStart)+ PortStart,
		   Conn0#{port => RP};
	       {ok, Conn0} ->
		   Conn0
	   end,
    #{ ip := Ip0
     , port := Port
     , acceptors := Acceptors 
     } = Conn,
    {ok, Ip} = inet:parse_ipv4_address(Ip0),
    {ok, Socket} = 
	gen_tcp:listen( Port
		      , [{ip, Ip}] ++ ?BINGO_TCP_DEFAULT_OPTS
		      ),
    {ok, _} = bingo_tcp_acceptor:start_link(Socket, Acceptors),
    ok = bingo_conf:set(conn, Conn),
    {ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Private Functions
%%%===================================================================
