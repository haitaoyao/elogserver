%%% -------------------------------------------------------------------
%%% Author  : haitao
%%% Description :
%%%
%%% Created : 2012-5-7
%%% -------------------------------------------------------------------
-module(els_log_writer).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("elogserver.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, write_log/2, rotate_file/1, get_file_path/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {file_path, file_handle}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(FilePath) ->
	gen_server:start_link(?MODULE, [FilePath], []).

write_log(WriterPid, LogData) ->
	gen_server:cast(WriterPid, {write_log, LogData}).

rotate_file(WriterPid) ->
	gen_server:cast(WriterPid, {rotate_file}).

get_file_path(WriterPid) ->
	gen_server:call(WriterPid, {get_file_path}).

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
init([FilePath]) ->
	{ok, FileHandle} = open_file(FilePath),
%% 	els_logs_repo:register_writer(FilePath, self()),
    {ok, #state{file_path = FilePath, file_handle = FileHandle}}.

open_file(FilePath) ->
	filelib:ensure_dir(FilePath),
	file:open(FilePath, [raw, append]).
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
handle_call({get_file_path}, _From, State = #state{file_path = FilePath}) ->
	{reply, FilePath, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({rotate_file}, State = #state{file_handle = FileHandle, file_path = FilePath}) ->
	file:close(FileHandle),
	rename_file(FilePath),
	{ok, NewFileHandle} = open_file(FilePath),
    {noreply, State#state{file_handle = NewFileHandle}};

handle_cast({write_log, Log}, State = #state{file_handle = FileHandle}) ->
	case file:write(FileHandle, Log) of
		ok ->
			{noreply, State};
		{error, Reason} ->
			error_logger:error_info("failed to wriete file, reason: " ++ atom_to_list(Reason)),
			{noreply, State}
	end;

handle_cast(_Msg, State) ->
    {noreply, State}.

rename_file(FilePath) ->
	{Year, Month, Day} = erlang:date(),
	NewFilePath = FilePath ++ "." ++ io_lib:format("~w-~w-~w", [Year, Month, Day]), 
	case filelib:is_file(NewFilePath) of
		true ->
			rename_file_iterate(FilePath, NewFilePath, 1, 1000);
		false ->
			file:rename(FilePath, NewFilePath)
	end.

rename_file_iterate(FilePath, NewPath, Max, Max) ->
	NewPath1 = NewPath ++ "." ++ integer_to_list(Max),
	case filelib:is_file(NewPath1) of
		true ->
			error_logger:error_msg("failed to rename file: " ++ FilePath ++ ", reason: max index exceeded");
		false ->
			file:rename(FilePath, NewPath1)
	end;
rename_file_iterate(FilePath, NewPath, Index, Max) ->
	NewPath1 = NewPath ++ "." ++ integer_to_list(Index),
	case filelib:is_file(NewPath1) of
		true ->
			rename_file_iterate(FilePath, NewPath, Index + 1, Max);
		false ->
			file:rename(FilePath, NewPath1)
	end.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State = #state{file_handle = FileHandle, file_path = FilePath}) ->
	els_logs_repo:deregister_writer(FilePath, self()),
	file:close(FileHandle),
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

-ifdef(TEST).
rename_file_test() ->
	Path = "/tmp/aa",
	file:write_file(Path, "hello"),
	rename_file(Path),
	ok.
-endif.