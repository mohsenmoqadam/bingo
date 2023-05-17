-module(bingo_api_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("bingo.hrl").

suite() ->
    [{timetrap, {seconds,60}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(bingo),

    %% === Simple Delay
    timer:sleep(1000),
    
    
    [ {node_1, node()}
    , {node_2, 'test_node_2@127.0.0.1'} 
    , {node_3, 'test_node_3@127.0.0.1'}
    , {not_available_node, 'not_available_node@127.0.0.1'}
      | Config
    ].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [ ping
    , call
    , cast
    , multi_ping
    , multi_call
    , multi_cast
    ].

ping(Config) ->
    Node = ?config(node_1, Config),
    NANode = ?config(not_available_node, Config),
    
    {pong, Node} = bingo:ping(Node),
    {error, not_available} = bingo:ping(NANode),
    
    ok.

call(Config) ->
    Node = ?config(node_2, Config),
    NANode = ?config(not_available_node, Config),
    
    %% call/2
    true = bingo:call(Node, is_alive),
    {error, not_available} = bingo:call(NANode, is_alive),
    
    %% call/3
    true = bingo:call(Node, is_alive, 100),
    "echo/0" = bingo:call(Node, bingo, echo),
    {error, not_available} = bingo:call(NANode, is_alive, 100),
    {error, not_available} = bingo:call(NANode, bingo, echo),

    %% call/4
    "echo/0" = bingo:call(Node, bingo, echo, 100),
    hello = bingo:call(Node, bingo, echo, [hello]),
    {error, not_available} = bingo:call(NANode, bingo, echo, 100),
    {error, not_available} = bingo:call(NANode, bingo, echo, [hello]),
    
    %% call/5
    hello = bingo:call(Node, bingo, echo, [hello], 100),
    {error, not_available} = bingo:call(NANode, bingo, echo, [hello], 100),
    
    ok.

cast(Config) ->
    Node = ?config(node_2, Config),
    NANode = ?config(not_available_node, Config),

    %% cast/2
    ok = bingo:cast(Node, is_alive),
    {error, not_available} = bingo:cast(NANode, is_alive),

    %% cast/3
    ok = bingo:cast(Node, is_alive, 100),
    ok = bingo:cast(Node, bingo, echo),
    {error, not_available} = bingo:cast(NANode, is_alive, 100),
    {error, not_available} = bingo:cast(NANode, bingo, echo),

    %% cast/4
    ok = bingo:cast(Node, bingo, echo, 100),
    ok = bingo:cast(Node, bingo, echo, [hello]),
    {error, not_available} = bingo:cast(NANode, bingo, echo, 100),
    {error, not_available} = bingo:cast(NANode, bingo, echo, [hello]),

    %% cast/5
    ok = bingo:cast(Node, bingo, echo, [hello], 100),
    {error, not_available} = bingo:cast(NANode, bingo, echo, [hello], 100),

    ok.

multi_ping(Config) ->
    Node1 = ?config(node_2, Config),
    Node2 = ?config(node_3, Config),
    NANode = ?config(not_available_node, Config),

    { ok
    , #{ Node1 := pong
       , Node2 := pong
       , NANode := not_available
       }
    } = bingo:multi_ping([Node1, Node2, NANode]),
    ok.

multi_call(Config) ->
    Node1 = ?config(node_2, Config),
    Node2 = ?config(node_3, Config),
    NANode = ?config(not_available_node, Config),

    %% multi_call/2
    { ok
    , #{ Node1 := true
       , Node2 := true
       , NANode := not_available
       }
    } = bingo:multi_call([Node1, Node2, NANode], is_alive),

    %% multi_call/3
    { ok
    , #{ Node1 := true
       , Node2 := true
       , NANode := not_available
       }
    } = bingo:multi_call([Node1, Node2, NANode], is_alive, 100),
    { ok
    , #{ Node1 := "echo/0"
       , Node2 := "echo/0"
       , NANode := not_available
       }
    } = bingo:multi_call([Node1, Node2, NANode], bingo, echo),

    %% multi_call/4
    { ok
    , #{ Node1 := "echo/0"
       , Node2 := "echo/0"
       , NANode := not_available
       }
    } = bingo:multi_call([Node1, Node2, NANode], bingo, echo, 100),
    { ok
    , #{ Node1 := hello
       , Node2 := hello
       , NANode := not_available
       }
    } = bingo:multi_call([Node1, Node2, NANode], bingo, echo, [hello]),

    %% multi_call/5
    { ok
    , #{ Node1 := hello
       , Node2 := hello
       , NANode := not_available
       }
    } = bingo:multi_call([Node1, Node2, NANode], bingo, echo, [hello], 100),
    
    ok.

multi_cast(Config) ->
    Node1 = ?config(node_2, Config),
    Node2 = ?config(node_3, Config),
    NANode = ?config(not_available_node, Config),

    %% multi_cast/2
    { ok
    , #{ Node1 := ok
       , Node2 := ok
       , NANode := not_available
       }
    } = bingo:multi_cast([Node1, Node2, NANode], is_alive),

    %% multi_cast/3
    { ok
    , #{ Node1 := ok
       , Node2 := ok
       , NANode := not_available
       }
    } = bingo:multi_cast([Node1, Node2, NANode], is_alive, 100),
    { ok
    , #{ Node1 := ok
       , Node2 := ok
       , NANode := not_available
       }
    } = bingo:multi_cast([Node1, Node2, NANode], bingo, echo),

    %% multi_cast/4
    { ok
    , #{ Node1 := ok
       , Node2 := ok
       , NANode := not_available
       }
    } = bingo:multi_cast([Node1, Node2, NANode], bingo, echo, 100),
    { ok
    , #{ Node1 := ok
       , Node2 := ok
       , NANode := not_available
       }
    } = bingo:multi_cast([Node1, Node2, NANode], bingo, echo, [hello]),

    %% multi_cast/5
    { ok
    , #{ Node1 := ok
       , Node2 := ok
       , NANode := not_available
       }
    } = bingo:multi_cast([Node1, Node2, NANode], bingo, echo, [hello], 100),
    
    ok.
