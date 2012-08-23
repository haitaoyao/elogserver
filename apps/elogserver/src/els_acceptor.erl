%% Author: haitao
%% Created: 2012-4-24
%% Description: TODO: Add description to els_acceptor
-module(els_acceptor).

%%
%% Include files
%%
-define(ACCEPT_TIMEOUT, 2000).
%%
%% Exported Functions
%%
-export([start_link/1, init/1]).

%%
%% API Functions
%%
start_link(LSocket) ->
	Pid = proc_lib:spawn(?MODULE, init, [LSocket]),
	{ok, Pid}.

init(LSocket) ->
	case gen_tcp:accept(LSocket, ?ACCEPT_TIMEOUT) of
		{ok, Socket} ->
			els_handler_sup:new_connection(Socket),
		        lager:log(info, self(), io_lib:format("new connection, source: ~s", [els_utils:client_address(Socket)])),
			?MODULE:init(LSocket);
		{error, timeout} ->
			?MODULE:init(LSocket);
		{error, closed} ->
			els_transport:acceptor_died(self()),
			exit(normal);
		{error, Reason} ->
			els_transport:acceptor_died(self()),
			exit(Reason)
	end.
%%
%% Local Functions
%%

