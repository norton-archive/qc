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
-export([qc_run/0, qc_run/1, qc_run/2]).
-export([qc_sample/0, qc_sample/1, qc_prop/0, qc_prop/1]).
-export([qc_counterexample/0, qc_counterexample/1, qc_counterexample/2]).
-export([qc_counterexample_read/1, qc_counterexample_write/1, qc_counterexample_write/2]).

-include("qc.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec qc_run() -> boolean().
qc_run() ->
    qc_run(500).

-spec qc_run(non_neg_integer()) -> boolean().
qc_run(NumTests) ->
    qc_run(NumTests, []).

-spec qc_run(non_neg_integer(), [parallel | noshrink | {sometimes,pos_integer()} | any()]) -> boolean().
qc_run(NumTests, Options) ->
    case proplists:get_bool(noshrink, Options) of
        false ->
            ?QC:quickcheck(numtests(NumTests,qc_prop(Options)));
        true ->
            ?QC:quickcheck(numtests(NumTests,noshrink(qc_prop(lists:delete(noshrink, Options)))))
    end.

%% sample
qc_sample() ->
    qc_sample([]).

qc_sample(Options) ->
    qc_statem:qc_sample(?MODULE, Options).

%% prop
qc_prop() ->
    qc_prop([]).

qc_prop(Options) ->
    qc_statem:qc_prop(?MODULE, Options).

%% counterexample
qc_counterexample() ->
    qc_counterexample([]).

qc_counterexample(Options) ->
    qc_counterexample(Options, ?QC:counterexample()).

qc_counterexample(Options, CounterExample) ->
    ?QC:check(qc_prop(Options), CounterExample).

%% counterexample read
qc_counterexample_read(FileName) ->
    {ok, [CounterExample]} = file:consult(FileName),
    qc_counterexample([], CounterExample).

%% counterexample write
qc_counterexample_write(FileName) ->
    qc_counterexample_write(FileName, ?QC:counterexample()).

qc_counterexample_write(FileName, CounterExample) ->
    file:write_file(FileName, io_lib:format("~p.", [CounterExample])).

-endif. %% -ifdef(qc_statem).

