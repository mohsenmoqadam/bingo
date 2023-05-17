-module(bingo_mc_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    SupFlags = #{strategy => one_for_one,
		 intensity => 10,
		 period => 1},

    {ok, McWorkers} = bingo_conf:get('mc.workers'),
    McWorkerSpecs = [ begin
			  Id = list_to_atom( "bingo_mc_worker_"
					     ++ integer_to_list(I) ),
			  #{ id => Id
			   , start => { bingo_mc_worker
				      , start_link
				      , [Id] 
				      }
			   , restart => permanent
			   , shutdown => 5000
			   , type => worker
			   , modules => [bingo_mc_worker]
			   }
		      end || I <- lists:seq(1, McWorkers)], 
    {ok, {SupFlags, McWorkerSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
