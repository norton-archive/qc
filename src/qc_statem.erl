%%%-------------------------------------------------------------------
%%% Copyright (c) 2010-2012 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : qc_statem.erl
%%% Purpose : Wrapper for statem
%%%-------------------------------------------------------------------

-module(qc_statem).

-ifdef(QC).

-include("qc_statem.hrl").

%% API
-export([qc_run/3]).
-export([qc_sample/2]).
-export([qc_prop/2]).
-export([qc_counterexample/3]).
-export([qc_counterexample_read/3]).
-export([qc_counterexample_write/2]).

%% Interface Functions
-export([behaviour_info/1]).

%% Define the behaviour's required mods.
behaviour_info(callbacks) ->
    [{scenario_gen,0}
     , {command_gen,1}
     , {initial_state,1}
     , {state_is_sane,1}
     , {next_state,3}
     , {precondition,2}
     , {postcondition,3}
     , {setup,0}
     , {setup,1}
     , {teardown,2}
     , {aggregate,1}
    ];
behaviour_info(_Other) ->
	undefined.

%%%----------------------------------------------------------------------
%%% types and records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
qc_run(Module, NumTests, Options) ->
    (impl(Module)):qc_run(NumTests, Options).

qc_sample(Module, Options) ->
    (impl(Module)):qc_sample(Options).

qc_prop(Mod, Options) ->
    (impl(Mod)):qc_prop(Options).

qc_counterexample(Module, Options, CounterExample) ->
    ?QC:check(qc_prop(Module, Options), CounterExample).

qc_counterexample_read(Module, Options, FileName) ->
    {ok, [CounterExample]} = file:consult(FileName),
    qc_counterexample(Module, Options, CounterExample).

qc_counterexample_write(FileName, CounterExample) ->
    file:write_file(FileName, io_lib:format("~p.~n", CounterExample)).

%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------
impl(Module) ->
    qc_statem_impl:new(Module).

-endif. %% -ifdef(QC).
