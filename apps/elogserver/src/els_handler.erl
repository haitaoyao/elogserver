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
-export([start_link/1, recv_loop/2, log_rotated/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-define(READ_TIMEOUT, 4000).
-define(SERVER, ?MODULE).
-record(state, {socket, 
				file_path, file_id, 
			    client_address}).
%%
%% API Functions
%%
start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).

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
	new_recv_process(Socket),
	{ok, #state{socket = Socket,
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
handle_call(_Msg, _From, State) ->
	{noreply, State}.

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
handle_packet(1, Data, State) when is_binary(Data) ->
	DataString = binary_to_list(Data),
	[TopicName, FileName] = string:tokens(DataString, "##"),
	Folder = els_config:get_data_path() ++ "/" ++ TopicName,
	FilePath = Folder ++ "/" ++ FileName,
	els_logs_repo:register_connection(FilePath, self()),
	State1 = State#state{file_path = FilePath},
	State1;

%%
%% 2: means data came
%%
handle_packet(2, Data, State = #state{client_address = ClientAddress, file_path = FilePath}) when is_binary(Data) ->
	LogData = list_to_binary([ClientAddress, " ", Data, "\n"]),
	WriterPid = els_logs_repo:get_log_writer(FilePath),
	els_log_writer:write_log(WriterPid, LogData),
	State;

handle_packet(Other, Data, State = #state{client_address = ClientAddress}) when is_integer(Other) and is_binary(Data) ->
	error_logger:error_msg(io_lib:format("invalid request, type: ~p, client ip: ~p~n", [Other, ClientAddress])),
	State.

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
handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
	error_logger:error_info("recv died, pid: " ++ Pid),
	erlang:demonitor(MonitorRef),
	{stop, Reason, State}.

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

