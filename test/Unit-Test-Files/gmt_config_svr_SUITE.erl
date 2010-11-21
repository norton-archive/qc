%%%----------------------------------------------------------------------
%%% Copyright (c) 2007-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_config_svr_SUITE.erl
%%% Purpose : GMT config server test suite
%%%----------------------------------------------------------------------

-module(gmt_config_svr_SUITE).

%% Test cases
-export([start_test/0]).

-define(MUT, gmt_config_svr). % Module Under Test (a.k.a. DUT)
-define(ATM, ?MODULE). % Automatic Test Module (a.k.a. ATE)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Test Cases
%%

do_setup() ->
    ok = application:set_env(gmt, central_config, "../priv/central.conf"),
    [ ok = application:start(A) || A <- [sasl, gmt] ],
    ok = ?MUT:reload_config("../test/Unit-Test-Files/test1.config"),
    ok.

do_teardown() ->
    [ ok = application:stop(A) || A <- [gmt, sasl] ],
    ok = application:unset_env(gmt, central_config),
    ok.

start_test() ->
    ok = test_1_001(),
    ok = test_1_002(),
    ok.

test_1_001() ->
    ok = do_setup(),

    %% without namespace
    {ok, "value1"} = ?MUT:get_config_value("key1", "defval1"),
    {ok, "defval"} = ?MUT:get_config_value("notexist", "defval"),
    {ok, "value2"} = ?MUT:get_config_value("key2", "defval2"),
    {ok, "defval"} = ?MUT:get_config_value("notexist", "defval"),

    {ok, "value1"} = ?MUT:get_config_value("key1", "defval1", undefined),
    {ok, "defval"} = ?MUT:get_config_value("notexist", "defval", undefined),
    {ok, "value2"} = ?MUT:get_config_value("key2", "defval2", undefined),
    {ok, "defval"} = ?MUT:get_config_value("notexist", "defval", undefined),

    %% with namespace
    {ok, "value3"} = ?MUT:get_config_value("key3", "defval3", "namespace1"),
    {ok, "defval"} = ?MUT:get_config_value("notexist", "defval", "namespace1"),
    {ok, "value4"} = ?MUT:get_config_value("key4", "defval4", "foo/bar/comp"),
    {ok, "defval"} = ?MUT:get_config_value("notexist", "defval", "foo/bar/comp"),

    %% get all values belonging to the namespace as dict().
    {ok, Dict} = ?MUT:get_config_namespace_dict("namespace1"),
    %%io:format("Dict = ~p~n", [Dict]),
    {ok, "value3"} = dict:find(key3, Dict),
    {ok, "value3_2"} = dict:find(key3_2, Dict),
    {ok, "value3_3"} = dict:find(key3_3, Dict),
    {ok, "value3_4"} = dict:find(key3_4, Dict),
    error = dict:find(notexist, Dict),

    %% with defval
    {ok, "value3"} = ?MUT:get_config_value_from_dict(key3, "defval3", Dict),
    {ok, "defval"} = ?MUT:get_config_value_from_dict(notexist, "defval", Dict),

    ok = do_teardown(),
    ok.

test_1_002() ->
    ok = do_setup(),

    %% without namespace
    ok = ?MUT:set_config_value("key1", "setval1"),
    ok = ?MUT:set_config_value("key2", "setval2"),

    ok = ?MUT:set_config_value("key1", "setval1", undefined),
    ok = ?MUT:set_config_value("key2", "setval2", undefined),

    %% with namespace
    ok = ?MUT:set_config_value("key3", "setval3", "namespace1"),
    ok = ?MUT:set_config_value("key4", "setval4", "foo/bar/comp"),

    %% without namespace
    {ok, "setval1"} = ?MUT:get_config_value("key1", "setval1"),
    {ok, "setval"} = ?MUT:get_config_value("notexist", "setval"),
    {ok, "setval2"} = ?MUT:get_config_value("key2", "setval2"),
    {ok, "setval"} = ?MUT:get_config_value("notexist", "setval"),

    {ok, "setval1"} = ?MUT:get_config_value("key1", "setval1", undefined),
    {ok, "setval"} = ?MUT:get_config_value("notexist", "setval", undefined),
    {ok, "setval2"} = ?MUT:get_config_value("key2", "setval2", undefined),
    {ok, "setval"} = ?MUT:get_config_value("notexist", "setval", undefined),

    %% with namespace
    {ok, "setval3"} = ?MUT:get_config_value("key3", "setval3", "namespace1"),
    {ok, "setval"} = ?MUT:get_config_value("notexist", "setval", "namespace1"),
    {ok, "setval4"} = ?MUT:get_config_value("key4", "setval4", "foo/bar/comp"),
    {ok, "setval"} = ?MUT:get_config_value("notexist", "setval", "foo/bar/comp"),

    ok = do_teardown(),
    ok.
