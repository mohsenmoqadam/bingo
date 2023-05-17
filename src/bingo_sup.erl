-module(bingo_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    SupFlags = #{ strategy => one_for_one
		, intensity => 10
		, period => 1
		},
    BingoMngrSpec = #{ id => bingo_mngr  
		     , start => { bingo_mngr
				, start_link
				, []
				}
		     , shutdown => 3000
		     , type => worker
		     , modules => [bingo] 
		     },
    BingoListenerSpec = #{ id => bingo_tcp_listener  
			 , start => { bingo_tcp_listener
				    , start_link
				    , []
				    }
			 , shutdown => 3000
			 , type => worker
			 , modules => [bingo_tcp_listener] 
			 }, 
    BingoMcSup = #{ id => bingo_mc_sup  
		  , start => { bingo_mc_sup
			     , start_link
			     , []
			     }
		  , shutdown => 3000
		  , type => supervisor
		  , modules => [bingo_mc_sup] 
		  },

    {ok, {SupFlags, [BingoMngrSpec, BingoListenerSpec, BingoMcSup]} }.

%%====================================================================
%% Internal functions
%%====================================================================
