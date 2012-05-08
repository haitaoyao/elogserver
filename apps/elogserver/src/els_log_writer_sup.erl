%%% -------------------------------------------------------------------
%%% Author  : haitao
%%% Description :
%%%
%%% Created : 2012-5-7
%%% -------------------------------------------------------------------
-module(els_log_writer_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0, new_writer/1, get_running_writers/0]).

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
-define(HANDLER_MODULE, els_log_writer).
%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

%% create a new connection
new_writer(FilePath) ->
	supervisor:start_child(?MODULE, [FilePath]).

%% start the supervisor
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

get_running_writers() ->
	case erlang:whereis(?SERVER) of
		undefined ->
			[];
		Pid when is_pid(Pid) ->
			Children = supervisor:which_children(Pid),
			lists:foldl(fun({_Id, Child, _Type, _Modules}, Result) -> 
								[{els_log_writer:get_file_path(Child), Child}|Result] end, [], Children)
	end.

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

