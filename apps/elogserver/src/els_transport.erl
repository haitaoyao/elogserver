%%% -------------------------------------------------------------------
%%% Author  : haitao
%%% Description :
%%%
%%% Created : 2012-4-24
%%% -------------------------------------------------------------------
-module(els_transport).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, start_link/1, acceptor_died/1]).
-define(DEFAULT_PORT, 9512).
-define(SERVER, ?MODULE).
-define(TCP_OPTIONS, [binary,
                {reuseaddr, true},
                {packet, 0},
                {active, false}
			   ]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(server_state, {port, lsocket}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	start_link(?DEFAULT_PORT).

start_link(Port) when is_integer(Port) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

acceptor_died(Pid) ->
	gen_server:cast(?SERVER, {acceptor_died, Pid}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Port]) ->
	process_flag(trap_exit, true),
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{error, Reason} ->
			{stop, Reason};
		{ok, LSocket} ->
			els_acceptor:start_link(LSocket),
			{ok, #server_state{port = Port, lsocket = LSocket}}
	end.
    

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
	error_logger:info_msg(io:format("unknown message for cast: ~p from ~p~n", [Request, From])),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({acceptor_died, _Pid}, State=#server_state{port = Port, lsocket = LSocket}) ->
	gen_tcp:close(LSocket),
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{error, Reason} ->
			{stop, Reason};
		{ok, LSocket1} ->
			{ok, State#server_state{lsocket = LSocket1}}
	end;
handle_cast(Msg, State) ->
	error_logger:info_msg(io:format("unknown message for cast: ~p~n", [Msg])),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'EXIT', _Pid, normal}, State) ->
    {stop, normal, State};
handle_info({'DOWN', _Pid, Reason}, State) ->
	{stop, Reason, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State=#server_state{lsocket = LSocket}) ->
	gen_tcp:close(LSocket),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

