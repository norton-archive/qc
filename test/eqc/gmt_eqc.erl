%%%-------------------------------------------------------------------
%%% Copyright (c) 2009-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_eqc.erl
%%% Purpose : Wrapper for eqc.erl
%%%-------------------------------------------------------------------

-module(gmt_eqc).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start/0]).
-export([silent/1]).
-export([write_counterexamples/1, write_counterexamples/2, write_counterexamples/3]).
-export([write_counterexample/3, write_counterexample/4]).
-export([eunit_module/1, eunit_module/2, eunit_module/3, eunit_module/4]).

%% @doc Starts (and possibly restarts) the QuickCheck server. If
%% another instance is not running, start the server and return the
%% server's process id.  If another instance is already running,
%% return true.  Otherwise, forcefully restart the server.
start() ->
    try
        eqc:start(false)
    catch
        exit:already_running ->
            true;
        _X:_Y ->
            eqc:start(true)
    end.

%% @doc Disable QuickCheck's test output (i.e. the "dots")
silent(Prop) ->
    Filter = fun($.) -> false; (_) -> true end,
    on_output(fun(Fmt, Args) -> io:format(lists:filter(Filter,Fmt), Args), ok end, Prop).

%% @doc Write failing counterexamples for specified Module
write_counterexamples(Module) ->
    write_counterexamples(Module, eqc:counterexamples()).

write_counterexamples(Module, CounterExamples) ->
    write_counterexamples(Module, CounterExamples, calendar:local_time()).

write_counterexamples(Module, CounterExamples, LocalTime) ->
    [ write_counterexample(Module, Prop, CE, LocalTime) || {Prop, CE} <- CounterExamples ].

write_counterexample(Module, Prop, CounterExample) ->
    write_counterexample(Module, Prop, CounterExample, calendar:local_time()).

write_counterexample(Module, Prop, CounterExample, {{Year,Month,Day},{Hour,Minute,Second}}) ->
    Fmt = "~s:~s-~4..0B~2..0B~2..0B-~2..0B~2..0B~2..0B.erl",
    Args = [Module,Prop,Year,Month,Day,Hour,Minute,Second],
    FileName = lists:flatten(io_lib:format(Fmt, Args)),
    ok = file:write_file(FileName, io_lib:format("~p.", [CounterExample])),
    FileName.

%% @doc Wrap eqc:module as an EUnit test fixture
eunit_module(Module) ->
    eunit_module(Module, 3000).

eunit_module(Module, NumTests) ->
    eunit_module(Module, NumTests, 60).

eunit_module(Module, NumTests, Timeout) ->
    eunit_module(Module, NumTests, Timeout, fun() -> noop end).

eunit_module(Module, NumTests, Timeout, Teardown) ->
    {setup, fun() -> eunit_setup(Module) end
     , fun(Mod) -> Teardown(), eunit_teardown(Mod) end
     , {timeout, Timeout, [fun() -> eunit_run(Module, NumTests) end]}
    }.

eunit_setup(Module) ->
    start(),
    Module.

eunit_teardown(Module) ->
    ?assertEqual([], write_counterexamples(Module)).

eunit_run(Module, NumTests) ->
    erlang:group_leader(whereis(user), self()),
    eqc:module([{numtests,NumTests}, fun eqc_gen:noshrink/1, fun silent/1], Module).

-endif. %% -ifdef(EQC).
