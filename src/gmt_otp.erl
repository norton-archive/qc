
%%%----------------------------------------------------------------------
%%% Copyright (c) 2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_otp.erl
%%% Purpose : GMT otp utilities
%%%----------------------------------------------------------------------

-module(gmt_otp).

%% API
-export([reload_config/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

reload_config() ->
    case init:get_argument(config) of
        {ok, [ Files ]} ->
            ConfFiles = [begin
                             S = filename:basename(F,".config"),
                             filename:join(filename:dirname(F),
                                           S ++ ".config")
                         end || F <- Files],
            %% Move sys.config to the head of the list
            Config = lists:sort(fun("sys.config", _) -> true;
                                   (_, _) -> false end, ConfFiles),

            OldEnv = application_controller:prep_config_change(),

            Apps = [{application, A, make_appl(A)}
                    || {A,_,_} <- application:which_applications()],
            application_controller:change_application_data(Apps, Config),
            application_controller:config_change(OldEnv);
        _ ->
            {ok, []}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_appl(App) when is_atom(App) ->
    AppList  = element(2,application:get_all_key(App)),
    FullName = code:where_is_file(atom_to_list(App) ++ ".app"),
    case file:consult(FullName) of
        {ok, [{application, _, Opts}]} ->
            Env = proplists:get_value(env, Opts, []),
            lists:keyreplace(env, 1, AppList, {env, Env});
        {error, _Reason} ->
            lists:keyreplace(env, 1, AppList, {env, []})
    end.
