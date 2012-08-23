
-module(els_utils).

-compile([export_all]).

client_address(Socket) ->
	{ok, {{A, B, C, D}, Port}} = inet:peername(Socket),
	io_lib:format("~p.~p.~p.~p:~p", [A, B, C, D, Port]).
