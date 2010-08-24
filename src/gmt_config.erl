%%%----------------------------------------------------------------------
%%% Copyright (c) 2006-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% File    : gmt_config.erl
%%% Purpose : GMT config utilities
%%%----------------------------------------------------------------------

-module(gmt_config).

-export([get_config_file_path/0
         , get_config_dir/0
         , read_kv_config_file/0
         , read_kv_config_file/1
         , read_kv_config_file/2
         , get_config_value/2
         , get_config_value/3
         , get_config_value/4
         , get_config_value_i/2
         , get_config_value_i/3]).


%% TODO: Incomplete!  100% of public API is not yet covered.

-spec(get_config_value/2 :: (atom(), term()) ->
             term() ).
-spec(get_config_value_i/2 :: (atom(), integer()) ->
             integer() ).


%% @spec () -> string()
%% @doc Return central config file path, using a default if env is unspecified.

get_config_file_path() ->
    case init:get_argument(central_config) of
        {ok, [[Path]]} -> Path;
        _              -> throw({no_init_arg_defined, central_config})
    end.

get_config_dir() ->
    case init:get_argument(central_config) of
        {ok, [[Path]]} -> drop_central_config(Path);
        _              -> throw({no_init_arg_defined, central_config})
    end.

drop_central_config(Path) ->
    string:substr(Path, 1, string:str(Path, "/central.conf")).

%% @spec () -> {ok, kv_list()} | {error, Reason}
%% @equiv read_kv_config_file(get_config_file_path())

read_kv_config_file() ->
    read_kv_config_file(get_config_file_path()).

%% @spec (Path::string()) -> {ok, kv_list()} | {error, Reason}
%% @doc Read and parse a simple Cyrus-style key-value config file.

read_kv_config_file(Path) ->
    read_kv_config_file(Path, undefined).

%% @spec (string(), atom()) -> {ok, kv_list()} | {error, Reason}
%% @doc Read and parse a simple Cyrus-style key-value config file subject
%% to Namespace

%%
%% TODO: Need unit tests for this method
%%

read_kv_config_file(Path, Namespace) ->
    StrNamespace = gmt_util:list_ify(Namespace),
    case file:read_file(Path) of
        {ok, B} ->
            Lines = string:tokens(binary_to_list(B), "\n"),
            F = fun([], Acc) ->
                        Acc;
                   ([$#|_], Acc) ->
                        Acc;
                   (Line, Acc) ->
                        case string:str(Line, ":") of
                            N when N < 2 -> Acc;
                            P -> Left = string:substr(Line, 1, P - 1),
                                 R1 = string:substr(Line, P + 1),
                                 Right = string:strip(R1),
                                 %% check Namespace
                                 case Namespace of
                                     undefined ->
                                         %% skip Namespace
                                         New = {list_to_atom(Left), Right},
                                         [New|Acc];
                                     _ ->
                                         case string:str(Left, ".") of
                                             M when M < 2 -> Acc;
                                             O -> Left1 = string:substr(Left, 1, O - 1),
                                                  R2 = string:substr(Left, O + 1),
                                                  Right1 = string:strip(R2),
                                                  case Left1 of
                                                      StrNamespace ->
                                                          %% strip Namespace
                                                          New = {list_to_atom(Right1), Right},
                                                          [New|Acc];
                                                      _ -> Acc
                                                  end
                                         end
                                 end
                        end
                end,
            L = lists:foldl(F, [], Lines),
            {ok, L};
        Err ->
            Err
    end.

%% @spec (Name::atom(), Default::term()) -> string()
%% @equiv get_config_value(get_config_file_path(), Name, Default)

get_config_value(Name, Default) ->
    get_config_value(get_config_file_path(), Name, Default).

%% @spec (Path::string(), Name::atom(), Default::term()) -> string()
%% @doc Get a config file value or return the default.
%% The default must be a string, because the value we return from the
%% config file will also be a string -- it's the caller's responsibility
%% to convert to a integer, atom, or whatever.

get_config_value(Path, Name, Default) ->
    get_config_value(Path, Name, Default, undefined).

%% @spec (string(), atom(), term(), atom()) -> string()
%% @doc Get a config file value subject to Namespace or return the default
%% The default must be a string, because the value we return from the
%% config file will also be a string -- it's the caller's responsibility
%% to convert to a integer, atom, or whatever.

get_config_value(Path, Name, Default, Namespace) ->
    case read_kv_config_file(Path, Namespace) of
        {ok, ConfList} ->
            case lists:keyfind(Name, 1, ConfList) of
                {_, Val} -> Val;
                _        -> Default
            end;
        _ ->
            Default
    end.

%% @spec (atom(), string() | integer()) -> string()
%% @equiv integer_to_list(get_config_value(Name, Default))

get_config_value_i(Name, Default) ->
    get_config_value_i(Name, Default, undefined).

%% @spec (atom(), string() | integer(), atom()) -> string()
%% @equiv integer_to_list(get_config_value(Name, Default))

get_config_value_i(Name, Default, Namespace) when is_list(Default) ->
    list_to_integer(get_config_value(get_config_file_path(), Name, Default, Namespace));
get_config_value_i(Name, Default, Namespace) when is_integer(Default) ->
    list_to_integer(get_config_value(get_config_file_path(), Name, integer_to_list(Default), Namespace)).

%%%
%%% Misc edoc stuff
%%%

%% @type kv_list() = list({atom(), string()})
