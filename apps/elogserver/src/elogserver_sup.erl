
-module(elogserver_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	WriterSupSpec = ?CHILD(els_log_writer_sup, supervisor),
	HandlerSupSpec = ?CHILD(els_handler_sup, supervisor),
	HandlerRepoSpec = ?CHILD(els_logs_repo, worker),
	TransportSpec = ?CHILD(els_transport, worker),
    {ok, { {one_for_one, 5, 10}, [WriterSupSpec, HandlerSupSpec, HandlerRepoSpec, TransportSpec]} }.

