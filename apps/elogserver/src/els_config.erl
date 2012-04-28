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
-define(CONFIG_FILE_PATH, "/data/config/elogserver/elogserver.config").
-define(DEFALUT_DATA_PATH, "/tmp/elogserver").
%%
%% API Functions
%%
get_config(Key) ->
	case filelib:is_file(?CONFIG_FILE_PATH) of
		true ->
			{ok, Configs} = file:consult(?CONFIG_FILE_PATH),
			case lists:keyfind(Key, 1, Configs) of
				false ->
					undefined;
				{Key, Data} ->
					Data
			end;
		false ->
			undefined
	end.


get_data_path() ->
	case get_config(data_path) of
		undefined ->
			?DEFALUT_DATA_PATH;
		Path ->
			Path
	end.


%%
%% Local Functions
%%

