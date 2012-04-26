%% Author: haitao
%% Created: 2012-4-24
%% Description: TODO: Add description to els_config
-module(els_config).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_config/1, get_data_path/0]).
%%
%% API Functions
%%
get_config(Key) when is_list(Key) ->
	"/tmp/elogserver".
%% 	case application:get_env(Key) of
%% 		undefined ->
%% 			"/tmp/elogserver";
%% 		{ok, Val} ->
%% 			Val
%% 	end,
%% 	Value.

get_data_path() ->
	get_config("data_path").

%%
%% Local Functions
%%

