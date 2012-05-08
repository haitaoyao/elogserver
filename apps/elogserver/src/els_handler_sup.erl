%%% -------------------------------------------------------------------
%%% Author  : haitao
%%% Description :
%%%
%%% Created : 2012-4-24
%%% -------------------------------------------------------------------
-module(els_handler_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External export
%% --------------------------------------------------------------------
-export([new_connection/1, start_link/0, get_all_handlers/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(HANDLER_MODULE, els_handler).
%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

%% create a new connection
new_connection(Socket) ->
	supervisor:start_child(?MODULE, [Socket]).

get_all_handlers() ->
	case erlang:whereis(?SERVER) of
		undefined ->
			[];
		Pid when is_pid(Pid) ->
			Children = supervisor:which_children(Pid),
			lists:foldl(fun({_Id, Child, _Type, _Modules}, Result) -> 
								[{els_handler:get_file_path(Child), Child}|Result] end, [], Children)
	end.

%% start the supervisor
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	HandlerSpec = {?HANDLER_MODULE, 
				   {?HANDLER_MODULE, start_link, []}, 
				   temporary, brutal_kill, worker, [?HANDLER_MODULE]},
    {ok,{{simple_one_for_one,0,1}, [HandlerSpec]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

