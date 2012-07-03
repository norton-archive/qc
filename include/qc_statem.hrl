%%%-------------------------------------------------------------------
%%% Copyright (c) 2009-2012 Gemini Mobile Technologies, Inc.  All rights reserved.
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

%% boilerplate API
%% -export([qc_run/0, qc_run/1, qc_run/2, qc_run/3]).
%% -export([qc_sample/0, qc_sample/1, qc_sample/2, qc_prop/0, qc_prop/1, qc_prop/2]).
%% -export([qc_counterexample/0, qc_counterexample/1, qc_counterexample/2, qc_counterexample/3]).
%% -export([qc_counterexample_read/1, qc_counterexample_read/2, qc_counterexample_read/3]).
%% -export([qc_counterexample_write/1, qc_counterexample_write/2]).

-include("qc.hrl").

%%% ----------------------------------------------------------------------
%%%  boilerplate API
%%% ----------------------------------------------------------------------

%% -spec qc_run() -> boolean().
%% qc_run() ->
%%     qc_run(500).

%% -spec qc_run(non_neg_integer()) -> boolean().
%% qc_run(NumTests) ->
%%     qc_run(NumTests, []).

%% -spec qc_run(non_neg_integer(), [{name,string()} | cover | {cover,[module()]} | parallel | noshrink | {sometimes,pos_integer()} | any()]) -> boolean().
%% qc_run(NumTests, Options) ->
%%     qc_run(?MODULE, NumTests, Options).

%% -spec qc_run(module(), non_neg_integer(), [{name,string()} | cover | {cover,[module()]} | parallel | noshrink | {sometimes,pos_integer()} | any()]) -> boolean().
%% qc_run(Module, NumTests, Options) ->
%%     qc_statem:qc_run(Module, NumTests, Options).

%% %% sample
%% qc_sample() ->
%%     qc_sample([]).

%% qc_sample(Options) ->
%%     qc_sample(?MODULE, Options).

%% qc_sample(Module, Options) ->
%%     qc_statem:qc_sample(Module, Options).

%% %% prop
%% qc_prop() ->
%%     qc_prop([]).

%% qc_prop(Options) ->
%%     qc_prop(?MODULE, Options).

%% qc_prop(Module, Options) ->
%%     qc_statem:qc_prop(Module, Options).

%% %% counterexample
%% qc_counterexample() ->
%%     qc_counterexample([]).

%% qc_counterexample(Options) ->
%%     qc_counterexample(Options, ?QC:counterexample()).

%% qc_counterexample(Options, CounterExample) ->
%%     qc_counterexample(?MODULE, Options, CounterExample).

%% qc_counterexample(Module, Options, CounterExample) ->
%%     qc_statem:qc_counterexample(Module, Options, CounterExample).

%% %% counterexample read
%% qc_counterexample_read(FileName) ->
%%     qc_counterexample_read([], FileName).

%% qc_counterexample_read(Options, FileName) ->
%%     qc_counterexample_read(?MODULE, Options, FileName).

%% qc_counterexample_read(Module, Options, FileName) ->
%%     qc_statem:qc_counterexample_read(Module, Options, FileName).

%% %% counterexample write
%% qc_counterexample_write(FileName) ->
%%     qc_counterexample_write(FileName, ?QC:counterexample()).

%% qc_counterexample_write(FileName, CounterExample) ->
%%     qc_statem:qc_counterexample_write(FileName, CounterExample).

-endif. %% -ifdef(qc_statem).
