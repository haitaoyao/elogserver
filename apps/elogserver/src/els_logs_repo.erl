%%% -------------------------------------------------------------------
%%% Author  : haitao
%%% Description :
%%%
%%% Created : 2012-4-25
%%% handle the log rotate
%%% record all the log files open.
%%% -------------------------------------------------------------------
-module(els_logs_repo).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, register_connection/1, delete_connection/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {handlers}).
-define(SERVER, ?MODULE).
%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_connection(FileId) ->
	gen_server:call(?SERVER, {register_connection, FileId}).

delete_connection(FileId) ->
	gen_server:call(?SERVER, {delete_connection, FileId}).
	
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
init([]) ->
	start_cron(),
    {ok, #state{handlers = dict:new()}}.

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
handle_call({register_connection, FileId}, From, State = #state{handlers = Handlers}) ->
	case dict:find(FileId, Handlers) of
		error ->
			Pids = sets:new(),
			{reply, ok, State#state{handlers = dict:append(FileId, sets:add_element(From, Pids), Handlers)}};
		{ok, Val} ->
			{reply, ok, State#state{handlers = dict:append(FileId, sets:add_element(From, Val), Handlers)}}
	end;
handle_call({delete_connection, FileId}, From, State = #state{handlers = Handlers}) ->
	case dict:find(FileId, Handlers) of
		error ->
			State;
		{ok, Val} ->
			case sets:is_element(From, Val) of
				true ->
					State#state{handlers = dict:store(FileId, sets:del_element(From, Val), Handlers)}
			end
	end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({timeout, _Ref, crontab}, State = #state{handlers = Handlers}) ->
	start_cron(),
	Handlers = dict:fetch_keys(Handlers),
	rotate_log(Handlers),
	{noreply, State};
handle_info(Info, State) ->
	error_logger:error_info(io_lib:format("unknown info: ~p~n", [Info])),
    {noreply, State}.


rotate_log([]) ->
	ok;
rotate_log([Handler|Rest]) when is_pid(Handler)->
	els_handler:rotate_log(Handler),
	notify_handlers(Rest).
notify_handlers([]) ->
	ok;
notify_handlers([H|Rest]) when is_pid(H) ->
	els_hander:rotate_log(H),
	notify_handlers(Rest).

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
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

start_cron() ->
	Time = seconds_to_midnight(),
	erlang:start_timer(Time * 1000, self(), crontab).

seconds_to_midnight() ->
	{_Date, Time} = calendar:local_time(),
	NowSeconds = calendar:time_to_seconds(Time),
	DiffSeconds = 86400 - NowSeconds,
	DiffSeconds.
	
	
