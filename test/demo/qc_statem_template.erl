%%%-------------------------------------------------------------------
%%% Copyright (C) 2013 by Joseph Wayne Norton <norton@alum.mit.edu>
%%% Copyright (c) 2012 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : qc_statem_ets.erl
%%% Purpose : Template for QuickCheck and PropEr StateM(achine)
%%%-------------------------------------------------------------------

-module(qc_statem_template).

-ifdef(QC).

%% @NOTE For boilerplate exports, see "qc_statem.hrl"
-include("qc_statem.hrl").

-ifdef(QC_STATEM).

%% API
-export([qc_run/2]).
-export([qc_sample/1]).
-export([qc_prop/1]).
-export([qc_counterexample/2]).
-export([qc_counterexample_read/2]).
-export([qc_counterexample_write/2]).

%% qc_statem Callbacks
-behaviour(qc_statem).
-export([scenario_gen/0, command_gen/1]).
-export([initial_state/1, state_is_sane/1, next_state/3, precondition/2, postcondition/3]).
-export([setup/0, setup/1, teardown/1, teardown/2, aggregate/1]).

%% DEBUG -compile(export_all).

%%%----------------------------------------------------------------------
%%% defines, types, records
%%%----------------------------------------------------------------------

-record(state, {
          parallel=false :: boolean()
         }).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

qc_run(NumTests, Options) ->
    qc_statem:qc_run(?MODULE, NumTests, Options).

qc_sample(Options) ->
    qc_statem:qc_sample(?MODULE, Options).

qc_prop(Options) ->
    qc_statem:qc_prop(?MODULE, Options).

qc_counterexample(Options, CounterExample) ->
    qc_statem:qc_counterexample(?MODULE, Options, CounterExample).

qc_counterexample_read(Options, FileName) ->
    qc_statem:qc_counterexample_read(?MODULE, Options, FileName).

qc_counterexample_write(FileName, CounterExample) ->
    qc_statem:qc_counterexample_write(FileName, CounterExample).


%%%----------------------------------------------------------------------
%%% qc_statem Callbacks
%%%----------------------------------------------------------------------
scenario_gen() ->
    undefined.

command_gen(#state{parallel=false}=S) ->
    serial_command_gen(S);
command_gen(#state{parallel=true}=S) ->
    parallel_command_gen(S).

serial_command_gen(_S) ->
    todo.

parallel_command_gen(_S) ->
    todo.

-spec initial_state(term()) -> #state{}.
initial_state(_Scenario) ->
    ?LET(Parallel,parameter(parallel,false),
         #state{parallel=Parallel}).

-spec state_is_sane(#state{}) -> boolean().
state_is_sane(_S) ->
    true.

-spec next_state(#state{}, term(), tuple()) -> #state{}.
next_state(S, _V, {call,_,_,_}) ->
    S.

-spec precondition(#state{}, tuple()) -> boolean().
precondition(_S, {call,_,_,_}) ->
    true.

-spec postcondition(#state{}, tuple(), term()) -> boolean().
postcondition(_S, {call,_,_,_}, _Res) ->
    false.

-spec setup() -> ok.
setup() ->
    ok.

-spec setup(term()) -> {ok, term()}.
setup(_Scenario) ->
    {ok, unused}.

-spec teardown(term()) -> ok.
teardown(unused) ->
    ok.

-spec teardown(term(), #state{}) -> ok.
teardown(Ref, _State) ->
    teardown(Ref).

-spec aggregate([{integer(), term(), term(), #state{}}])
               -> [{atom(), atom(), integer() | term()}].
aggregate(L) ->
    [ {Cmd,filter_reply(Reply)} || {_N,{set,_,{call,_,Cmd,_}},Reply,_State} <- L ].

filter_reply({'EXIT',{Err,_}}) ->
    {error,Err};
filter_reply(_) ->
    ok.


%%%----------------------------------------------------------------------
%%% Internal - Generators
%%%----------------------------------------------------------------------


%%%----------------------------------------------------------------------
%%% Internal - Model
%%%----------------------------------------------------------------------


%%%----------------------------------------------------------------------
%%% Internal - Implementation
%%%----------------------------------------------------------------------

-endif. %% -ifdef(QC_STATEM).

-endif. %% -ifdef(QC).
