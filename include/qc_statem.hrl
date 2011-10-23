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
%%% File    : qc_statem.hrl
%%% Purpose : Wrapper for QuickCheck and Proper State Machine
%%%-------------------------------------------------------------------

-ifndef(qc_statem).
-define(qc_statem, true).

%% API
-export([run/0, run/1, run/2]).
-export([sample_commands/0, sample_commands/1, prop_commands/0, prop_commands/1]).
-export([counterexample_commands/0, counterexample_commands/1, counterexample_commands/2]).
-export([counterexample_commands_read/1, counterexample_commands_write/1, counterexample_commands_write/2]).

-include("qc.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec run() -> boolean().
run() ->
    run(500).

-spec run(non_neg_integer()) -> boolean().
run(NumTests) ->
    run(NumTests, []).

-spec run(non_neg_integer(), [parallel | noshrink]) -> boolean().
run(NumTests, Options) ->
    case proplists:get_bool(noshrink, Options) of
        false ->
            ?QC:quickcheck(numtests(NumTests,prop_commands(Options)));
        true ->
            ?QC:quickcheck(numtests(NumTests,noshrink(prop_commands(lists:delete(noshrink, Options)))))
    end.

%% sample commands
sample_commands() ->
    sample_commands([]).

sample_commands(Options) ->
    qc_statem:qc_sample_commands(?MODULE, Options).

%% prop commands
prop_commands() ->
    prop_commands([]).

prop_commands(Options) ->
    qc_statem:qc_run_commands(?MODULE, Options).

%% counterexample commands
counterexample_commands() ->
    counterexample_commands([]).

counterexample_commands(Options) ->
    counterexample_commands(Options, ?QC:counterexample()).

counterexample_commands(Options, CounterExample) ->
    ?QC:check(prop_commands(Options), CounterExample).

%% counterexample commands read
counterexample_commands_read(FileName) ->
    {ok, CounterExample} = file:consult(FileName),
    counterexample_commands(CounterExample).

%% counterexample commands write
counterexample_commands_write(FileName) ->
    counterexample_commands_write(FileName, ?QC:counterexample()).

counterexample_commands_write(FileName, CounterExample) ->
    file:write_file(FileName, io_lib:format("~p.", [CounterExample])).

-endif. %% -ifdef(qc_statem).

