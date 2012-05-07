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
-include("elogserver.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, register_connection/2, delete_connection/2, get_log_writer/1, 
		 register_writer/2, deregister_writer/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {handlers, writers}).
-define(SERVER, ?MODULE).
%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_connection(FilePath, Pid) ->
	gen_server:call(?SERVER, {register_connection, FilePath, Pid}).

delete_connection(FilePath, Pid) ->
	gen_server:cast(?SERVER, {delete_connection, FilePath, Pid}).

get_log_writer(FilePath) ->
	gen_server:call(?SERVER, {get_log_writer, FilePath}).

register_writer(FilePath, Pid) ->
	gen_server:call(?SERVER, {register_writer, FilePath, Pid}).

deregister_writer(FilePath, Pid) ->
	gen_server:call(?SERVER, {deregister_writer, FilePath, Pid}).

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
    {ok, #state{handlers = dict:new(), writers = dict:new()}}.

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
handle_call({register_connection, FilePath, Pid}, _From, State = #state{handlers = Handlers}) ->
	case dict:find(FilePath, Handlers) of
		error ->
			Pids = sets:new(),
			{reply, ok, State#state{handlers = dict:store(FilePath, sets:add_element(Pid, Pids), Handlers)}};
		{ok, Val} ->
			{reply, ok, State#state{handlers = dict:store(FilePath, sets:add_element(Pid, Val), Handlers)}}
	end;

handle_call({get_log_writer, FilePath}, _From, State = #state{writers = Writers}) ->
	case dict:find(FilePath, Writers) of
		{ok, Val} ->
			{reply, Val, State};
		error ->
			NewWriter = create_writer(FilePath),
			{reply, NewWriter, State#state{writers = dict:store(FilePath, NewWriter, Writers)}}
	end;

handle_call({register_writer, FilePath, WriterPid}, _From, State = #state{writers = Writers}) ->
	case dict:find(FilePath, Writers) of
		error ->
			error_logger:error_info("no mapping for log: " ++ FilePath ++ ", writer: " ++ WriterPid),
			NewWriters = dict:store(FilePath, WriterPid, Writers),
			{reply, ok, State#state{writers = NewWriters}};
		{ok, Val} ->
			case Val =:= WriterPid of
				true ->
					{reply, ok, State};
				false ->
					error_logger:error_msg("invalid writer mapping"),
					NewWriters = dict:store(FilePath, WriterPid, Writers),
					{reply, ok, State#state{writers = NewWriters}}
			end
	end;

handle_call({deregister_writer, FilePath, WriterPid}, _From, State = #state{writers = Writers}) ->
	case dict:find(FilePath, Writers) of
		error ->
			{noreply, State};
		{ok, Val} ->
			case Val =:= WriterPid of
				true ->
					{reply, ok, State#state{writers = dict:erase(FilePath, Writers)}};
				false ->
					error_logger:error_msg("invalid deregister_writer request, pid: " ++ WriterPid),
					{reply, ok, State}
			end
	end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

create_writer(FilePath) ->
	{ok, Pid} = els_log_writer_sup:new_writer(FilePath),
	Pid.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({delete_connection, FilePath, Pid}, State = #state{handlers = Handlers}) ->
	case dict:find(FilePath, Handlers) of
		error ->
			{noreply, State};
		{ok, Val} ->
			case sets:is_set(Val) of 
				true ->
					Value1 = sets:del_element(Pid, Val),
					{noreply, State#state{handlers = dict:store(FilePath, Value1, Handlers)}};
				false ->
					{noreply, State}
			end
	end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({timeout, _Ref, crontab}, State = #state{writers = Writers}) ->
	start_cron(),
	io:format("contab found~n"),
	dict:map(fun(_FilePath, Pid) -> els_log_writer:rotate_file(Pid) end, 
			 Writers),
	{noreply, State};
handle_info(Info, State) ->
	error_logger:error_info(io_lib:format("unknown info: ~p~n", [Info])),
	io:format("other message found: ~p~n " , [Info]),
    {noreply, State}.

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
%% 	Time = 10,
	erlang:start_timer(Time * 1000, self(), crontab).

seconds_to_midnight() ->
	{_Date, Time} = calendar:local_time(),
	NowSeconds = calendar:time_to_seconds(Time),
	DiffSeconds = 86400 - NowSeconds,
	DiffSeconds.
	
	
