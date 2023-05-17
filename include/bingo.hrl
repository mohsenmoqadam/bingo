
%% -*- mode:erlang -*-
-ifndef(HEADER_BINGO).
-define(HEADER_BINGO, true).

-define(BINGO_TCP_DEFAULT_OPTS, [ binary
				, {packet, 4}
				, {exit_on_close, true}
				, {nodelay, true}       % Send our requests immediately
				, {delay_send, false}   % Scheduler should favour timely delivery
				, {reuseaddr, true}     % Reuse local port numbers
				, {keepalive, true}     % Keep our channel open
				, {tos, 72}             % Deliver immediately
				, {active, true}        % Retrieve data from the socket upon request
				]
       ). 

-record( bingo_conn_info
       , { node
	 , ip
	 , port
	 , conns
	 , workers
	 }
       ).
-type bingo_conn_info() :: #bingo_conn_info{}.

-ifdef(TEST).
-define(LOG_ERROR(Format, Args), ct:print(default, 50, Format, Args)).
-define(LOG_INFO(Format, Args), ?LOG_ERROR(Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG_ERROR(Format, Args)).
-else.
-define(LOG_ERROR(Format, Args), lager:error(Format, Args)).
-define(LOG_INFO(Format, Args), lager:info(Format, Args)).
-define(LOG_DEBUG(Format, Args), lager:debug(Format, Args)).
-endif.

-endif.
