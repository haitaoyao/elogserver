%% Author: haitao
%% Created: 2012-4-24
%% Description: 
%% one ip, one server, n connections
-module(els_handler).
-behaviour(gen_server).

%%
%% Include files
%%
-include("elogserver.hrl").
%%
%% Exported Functions
%%
-export([start_link/1, recv_loop/2, new_connection/2, log_rotated/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-define(READ_TIMEOUT, 4000).
-define(SERVER, ?MODULE).
-record(state, {socket, file_handle, 
				file_path, file_id, recv_processes, connection_count = 0,
			    client_address}).
%%
%% API Functions
%%
start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).
	
new_connection(HandlerRef, Socket) ->
	gen_server:call(HandlerRef, {new_connection, Socket}).

log_rotated(Pid) ->
	gen_server:call(Pid, {log_rotated}).

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
init([Socket]) ->
	ClientAddress = client_address(Socket),
	RecvPid = new_recv_process(Socket),
	{ok, #state{socket = Socket,
				recv_processes = sets:add_element(RecvPid, sets:new()),
				connection_count = 1,
				client_address = ClientAddress}}.

recv_loop(Socket, Server) ->
	case gen_tcp:recv(Socket, 8, ?READ_TIMEOUT) of
		{error, timeout} ->
			?MODULE:recv_loop(Socket, Server);
		{error, Reason} ->
			recv_exit(Socket, Reason);
		{ok, Packet} ->
			case Packet of
				<<Header:32, DataLength:32>> ->
					{ok, Rest} = gen_tcp:recv(Socket, DataLength),
					gen_server:cast(Server, {handle_packet, Header, Rest}),
					?MODULE:recv_loop(Socket, Server)
			end
		end.

recv_exit(Socket, Msg) ->
	error_logger:error_info(Msg),
	gen_tcp:close(Socket),
	exit(Msg).
    

new_recv_process(Socket) ->
	%%  tell the server the client address
	RecvPid = proc_lib:spawn(?MODULE, recv_loop, [Socket, self()]),
	%% lock the socket to a specific process
	gen_tcp:controlling_process(Socket, RecvPid),
	erlang:monitor(process, RecvPid),
	RecvPid.
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
handle_call({log_rotated}, _From, State) ->
	swap_file(State),
	{noreply, State};
handle_call({new_connection, Socket}, 
			_From, 
			State = #state{connection_count = ConnectionCount,recv_processes = RecvProcesses}) ->
	RecvPid = new_recv_process(Socket),
	{reply, ok, State#state{connection_count = ConnectionCount + 1, 
							recv_processes = sets:add_element(RecvPid, RecvProcesses)}}.

swap_file(State = #state{file_handle = IoDevice, file_path = FilePath}) ->
	file:close(IoDevice),
	{ok, NewFileHandle} = open_file(FilePath),
	State#state{file_handle = NewFileHandle}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({handle_packet, Header, Data}, State) ->
	State1 = handle_packet(Header, Data, State),
	{noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%% 1: means hand shake
%% 
handle_packet(1, Data, State = #state{client_address = ClientAddress}) when is_binary(Data) ->
	DataString = binary_to_list(Data),
	[TopicName, FileName] = string:tokens(DataString, "##"),
	Folder = els_config:get_data_path() ++ "/" ++ TopicName,
	FilePath = Folder ++ "/" ++ FileName,
	FileId = ClientAddress ++ "##" ++ DataString,
	els_logs_repo:register_connection(FileId, self()),
	{ok, IoDevice} = open_file(FilePath),
	State1 = State#state{file_handle = IoDevice, file_path = FilePath, file_id = FileId},
	State1;

%%
%% 2: means data came
%%
handle_packet(2, Data, State = #state{file_handle = IoDevice, client_address = ClientAddress}) when is_binary(Data) ->
	case file:write(IoDevice, list_to_binary([ClientAddress, " ", Data, "\n"])) of
		ok ->
			State;
		{error, Reason} ->
			error_logger:error_info(Reason),
			State
	end;

handle_packet(Other, Data, State = #state{client_address = ClientAddress}) when is_integer(Other) and is_binary(Data) ->
	error_logger:error_msg(io_lib:format("invalid request, type: ~p, client ip: ~p~n", [Other, ClientAddress])),
	State.

open_file(FilePath) ->
	filelib:ensure_dir(FilePath),
	file:open(FilePath, [raw, append]).

client_address(Socket) ->
	{ok, {{A, B, C, D}, _Port}} = inet:peername(Socket),
	io_lib:format("~p.~p.~p.~p", [A, B, C, D]).
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'EXIT', _Pid, normal}, State) ->
    {stop, normal, State};
handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State = #state{recv_processes = RecvProcesses}) ->
	error_logger:error_info("recv died, pid: " ++ Pid),
	erlang:demonitor(MonitorRef),
	{stop, Reason, State#state{recv_processes = sets:del_element(Pid, RecvProcesses)}}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State = #state{file_id = FileId}) ->
	els_logs_repo:delete_connection(FileId, self()),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-ifdef(TEST).
rename_file_test() ->
	Path = "/tmp/aa",
	file:write_file(Path, "hello"),
	rename_file(Path),
	ok.
-endif.
