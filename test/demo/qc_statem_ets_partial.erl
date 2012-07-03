%%%-------------------------------------------------------------------
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
%%% File    : qc_statem_ets_partial.erl
%%% Purpose : Partial ETS test using QuickCheck and PropEr StateM(achine)
%%%-------------------------------------------------------------------

-module(qc_statem_ets_partial).

-ifdef(QC).

%% API
-export([qc_run/2]).
-export([qc_sample/1]).
-export([qc_prop/1]).
-export([qc_counterexample/2]).
-export([qc_counterexample_read/2]).
-export([qc_counterexample_write/2]).

%% qc_statem Callbacks
-behaviour(qc_statem).
-export([command_gen/2]).
-export([initial_state/0, state_is_sane/1, next_state/3, precondition/2, postcondition/3]).
-export([setup/1, teardown/1, teardown/2, aggregate/1]).

%% DEBUG -compile(export_all).
-export([new/2,
         insert/2,
         lookup/2,
         next/2
        ]).

%% @NOTE For boilerplate exports, see "qc_statem.hrl"
-include_lib("qc/include/qc_statem.hrl").


%%%----------------------------------------------------------------------
%%% defines, types, records
%%%----------------------------------------------------------------------

-define(TAB, ?MODULE).

%% {obj, term(), term()}
%% X#obj.key
%% X#obj.val
%% X#obj{key=foo}

-record(obj, {
          key :: term(),
          val :: term()
         }).

-record(state, {
          parallel=false :: boolean(),
          objs=[] :: [#obj{}],
          tab=undefined :: atom()
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

command_gen(Mod,#state{parallel=false}=S) ->
    serial_command_gen(Mod,S);
command_gen(Mod,#state{parallel=true}=S) ->
    parallel_command_gen(Mod,S).

serial_command_gen(_Mod,#state{tab=undefined}) ->
    {call,?MODULE,new,[?TAB, [named_table, public, ordered_set, {keypos,#obj.key}]]};
serial_command_gen(_Mod,#state{tab=Tab}=S) ->
    oneof([{call,?MODULE,insert,[Tab,gen_obj(S)]},
           {call,?MODULE,lookup,[Tab,gen_key(S)]},
           {call,?MODULE,next,[Tab,gen_key(S)]}
          ]).

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
next_state(S, V, {call,_,new,[_Tab, _Options]}) ->
    S#state{tab=V};
next_state(#state{objs=OldObjs}=S, _V, {call,_,insert,[_Tab, Objs]}) when is_list(Objs) ->
    S#state{objs=insert_objs(OldObjs, Objs)};
next_state(#state{objs=OldObjs}=S, _V, {call,_,insert,[_Tab, Obj]}) when is_tuple(Obj) ->
    S#state{objs=insert_objs(OldObjs, [Obj])};
next_state(S, _V, {call,_,_,_}) ->
    S.

-spec precondition(#state{}, tuple()) -> boolean().
precondition(_S, {call,_,_,_}) ->
    true.

-spec postcondition(#state{}, tuple(), term()) -> boolean().
postcondition(_S, {call,_,new,[Tab,_Options]}, Res) ->
    Res =:= Tab;
postcondition(_S, {call,_,insert,[_Tab,_Options]}, Res) ->
    Res;
postcondition(#state{objs=Objs}, {call,_,lookup,[_Tab,Key]}, Res) ->
    Res =:= [ Obj || #obj{key=K}=Obj <- Objs, K =:= Key ];
postcondition(#state{objs=Objs}, {call,_,next,[_Tab,Key]}, Res) ->
    case lists:dropwhile(fun(#obj{key=K}) -> K =< Key end, lists:sort(Objs)) of
        [] ->
            Res =:= '$end_of_table';
        %% [Obj|_T] ->
        %%     Res =:= element(2,Obj);
        %% [Obj|_T] ->
        %%     Res =:= element(#obj.key,Obj);
        %% [Obj|_T] ->
        %%     Res =:= Obj#obj.key;
        [#obj{key=K}|_T] ->
            Res =:= K
    end;
postcondition(_S, {call,_,_,_}, _Res) ->
    false.

-spec setup(boolean()) -> {ok, term()}.
setup(_Hard) ->
    catch ets:delete(?TAB),
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

gen_obj(S) ->
    ?LET(K,gen_key(S),
         #obj{key=K, val=gen_val()}).

gen_key() ->
    int().

gen_key(#state{objs=[]}) ->
    gen_key();
gen_key(#state{objs=Objs}) ->
    Keys = [ K || #obj{key=K} <- Objs ],
    frequency([{3, oneof(Keys)},
               {1, gen_key()}]).

gen_val() ->
    int().


%%%----------------------------------------------------------------------
%%% Internal - Model
%%%----------------------------------------------------------------------
insert_objs(OldObjs, []) ->
    OldObjs;
insert_objs(OldObjs, [#obj{key=K}=H|T]) ->
    NewObjs = [H|lists:keydelete(K, #obj.key, OldObjs)],
    insert_objs(NewObjs, T).


%%%----------------------------------------------------------------------
%%% Internal - Implementation
%%%----------------------------------------------------------------------

new(Tab, Options) ->
    catch ets:new(Tab, Options).

insert(Tab, ObjOrObjs) ->
    catch ets:insert(Tab, ObjOrObjs).

lookup(Tab, Key) ->
    catch ets:lookup(Tab, Key).

next(Tab, Key) ->
    catch ets:next(Tab, Key).

-endif. %% -ifdef(QC).
