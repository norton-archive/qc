%%%-------------------------------------------------------------------
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
%%% File    : qc_statem_ets.erl
%%% Purpose : Template for QuickCheck and PropEr StateM(achine)
%%%-------------------------------------------------------------------

-module(qc_statem_template).

-ifdef(QC).

%% qc_statem Callbacks
-behaviour(qc_statem).
-export([command_gen/2]).
-export([initial_state/0, state_is_sane/1, next_state/3, precondition/2, postcondition/3]).
-export([commands_setup/1, commands_teardown/1, commands_teardown/2, commands_aggregate/1]).

%% DEBUG -compile(export_all).

%% @NOTE For boilerplate exports, see "qc_statem.hrl"
-include_lib("qc/include/qc_statem.hrl").


%%%----------------------------------------------------------------------
%%% defines, types, records
%%%----------------------------------------------------------------------

-record(state, {
          parallel=false :: boolean()
         }).


%%%----------------------------------------------------------------------
%%% qc_statem Callbacks
%%%----------------------------------------------------------------------

command_gen(Mod,#state{parallel=false}=S) ->
    serial_command_gen(Mod,S);
command_gen(Mod,#state{parallel=true}=S) ->
    parallel_command_gen(Mod,S).

serial_command_gen(_Mod,_S) ->
    todo.

parallel_command_gen(_Mod,_S) ->
    todo.

-spec initial_state() -> #state{}.
initial_state() ->
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

-spec commands_setup(boolean()) -> {ok, term()}.
commands_setup(_Hard) ->
    {ok, unused}.

-spec commands_teardown(term()) -> ok.
commands_teardown(unused) ->
    ok.

-spec commands_teardown(term(), #state{}) -> ok.
commands_teardown(Ref, _State) ->
    commands_teardown(Ref).

-spec commands_aggregate([{integer(), term(), term(), #state{}}])
                        -> [{atom(), atom(), integer() | term()}].
commands_aggregate(L) ->
    [ Cmd || {_N,Cmd,_Reply,_State} <- L ].


%%%----------------------------------------------------------------------
%%% Internal - Generators
%%%----------------------------------------------------------------------


%%%----------------------------------------------------------------------
%%% Internal - Model
%%%----------------------------------------------------------------------


%%%----------------------------------------------------------------------
%%% Internal - Implementation
%%%----------------------------------------------------------------------

-endif. %% -ifdef(QC).
