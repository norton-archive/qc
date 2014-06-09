%%%-------------------------------------------------------------------
%%% Copyright (C) 2013-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
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
%%% Purpose : ETS test for QuickCheck and PropEr StateM(achine)
%%%-------------------------------------------------------------------

-module(qc_statem_ets).

-ifdef(QC).

%% @NOTE For boilerplate exports, see "qc_statem.hrl"
-include("qc_statem.hrl").

-ifdef(QC_STATEM).

%% API
-export([qc_run/0, qc_run/1, qc_run/2]).
-export([qc_sample/0, qc_sample/1]).
-export([qc_prop/1]).
-export([qc_check/0, qc_check/1, qc_check/2]).
-export([qc_check_file/2]).

%% qc_statem Callbacks
-behaviour(qc_statem).
-export([command/1]).
-export([initial_state/0, initial_state/1, next_state/3, invariant/1, precondition/2, postcondition/3]).
-export([init/0, init/1, stop/2, aggregate/1]).

%% DEBUG -compile(export_all).
%% Implementation
-export([%% ets
         new/2
        , insert/2
        , insert_new/2
        , delete/1
        , delete/2
        , delete_all_objects/1
        , lookup/2
        , first/1
        , next/2
        , info/2
        , tab2list/1
        ]).

%%%----------------------------------------------------------------------
%%% defines, types, records
%%%----------------------------------------------------------------------

-define(TAB, ?MODULE).
-define(INT_KEYS, lists:seq(0,10)).
-define(FLOAT_KEYS, [float(Key) || Key <- ?INT_KEYS]).
-define(BINARY_KEYS, [term_to_binary(Key) || Key <- ?INT_KEYS]).

-record(obj, {key :: integer() | float() | binary(), val :: integer() | float() | binary()}).

-type obj() :: #obj{}.
-type ets_type() :: set | ordered_set.  %% default is set
-type proplist() :: proplists:proplist().

-record(state, {
          parallel=false :: boolean(),
          type=undefined :: undefined | ets_type(),
          tab=undefined  :: undefined | tuple(),
          objs=[]        :: [obj()]
         }).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

qc_run() ->
    qc_run(100).

qc_run(NumTests) ->
    qc_run(NumTests, []).

qc_run(NumTests, Options) ->
    qc_statem:qc_run(?MODULE, NumTests, Options).

qc_sample() ->
    qc_sample([]).

qc_sample(Options) ->
    qc_statem:qc_sample(?MODULE, Options).

qc_prop(Options) ->
    qc_statem:qc_prop(?MODULE, Options).

qc_check() ->
    qc_check([]).

qc_check(Options) ->
    qc_check(Options, ?QC:counterexample()).

qc_check(Options, CounterExample) ->
    qc_statem:qc_check(?MODULE, Options, CounterExample).

qc_check_file(Options, FileName) ->
    qc_statem:qc_check_file(?MODULE, Options, FileName).


%%%----------------------------------------------------------------------
%%% qc_statem Callbacks
%%%----------------------------------------------------------------------
command(#state{parallel=false}=S) ->
    serial_command_gen(S);
command(#state{parallel=true}=S) ->
    parallel_command_gen(S).

serial_command_gen(#state{tab=undefined}=S) ->
    {call,?MODULE,new,[?TAB,gen_options(new,S)]};
serial_command_gen(#state{tab=Tab}=S) ->
    oneof([{call,?MODULE,insert,[Tab,oneof([gen_obj(S),gen_objs(S)])]}]
          ++ [{call,?MODULE,insert_new,[Tab,oneof([gen_obj(S),gen_objs(S)])]}]
          %% @TODO ++ [{call,?MODULE,delete,[Tab]}]
          ++ [{call,?MODULE,delete,[Tab,gen_key(S)]}]
          ++ [{call,?MODULE,delete_all_objects,[Tab]}]
          ++ [{call,?MODULE,lookup,[Tab,gen_key(S)]}]
          ++ [{call,?MODULE,first,[Tab]}]
          ++ [{call,?MODULE,next,[Tab,gen_key(S)]}]
          %% @TODO info
          ++ [{call,?MODULE,tab2list,[Tab]}]
         ).

parallel_command_gen(#state{tab=undefined}=S) ->
    {call,?MODULE,new,[?TAB,gen_options(new,S)]};
parallel_command_gen(#state{tab=Tab}=S) ->
    oneof([{call,?MODULE,insert,[Tab,oneof([gen_obj(S),gen_objs(S)])]}]
          ++ [{call,?MODULE,insert_new,[Tab,oneof([gen_obj(S),gen_objs(S)])]}]
          %% @TODO ++ [{call,?MODULE,delete,[Tab,gen_key(S)]}]
          ++ [{call,?MODULE,delete_all_objects,[Tab]}]
          ++ [{call,?MODULE,lookup,[Tab,gen_key(S)]}]
          ++ [{call,?MODULE,first,[Tab]}]
          ++ [{call,?MODULE,next,[Tab,gen_key(S)]}]
         ).

-spec initial_state() -> #state{}.
initial_state() ->
    initial_state([]).

-spec initial_state(proplist()) -> #state{}.
initial_state(Opts) ->
    #state{parallel=proplists:get_value(parallel, Opts, false)}.

-spec next_state(#state{}, term(), tuple()) -> #state{}.
next_state(#state{tab=undefined, type=undefined}=S, V, {call,_,new,[?TAB,Options]}) ->
    case [proplists:get_bool(X, Options) || X <- [set, ordered_set]] of
        [_, false] ->
            Type = set;
        [false, true] ->
            Type = ordered_set
    end,
    S#state{type=Type, tab=V};
next_state(S, _V, {call,_,insert,[_Tab,Objs]}) when is_list(Objs) ->
    insert_objs(S, Objs);
next_state(S, _V, {call,_,insert,[_Tab,Obj]}) ->
    insert_objs(S, [Obj]);
next_state(S, _V, {call,_,insert_new,[_Tab,Objs]}) when is_list(Objs) ->
    insert_new_objs(S, Objs);
next_state(S, _V, {call,_,insert_new,[_Tab,Obj]}) ->
    insert_new_objs(S, [Obj]);
next_state(S, _V, {call,_,delete,[_Tab]}) ->
    S#state{tab=undefined, objs=[]};
next_state(S, _V, {call,_,delete,[_Tab,Key]}) ->
    S#state{objs=keydelete(Key, S)};
next_state(S, _V, {call,_,delete_all_objects,[_Tab]}) ->
    S#state{objs=[]};
next_state(S, _V, {call,_,_,_}) ->
    S.

-spec invariant(#state{}) -> boolean().
invariant(_S) ->
    true.

-spec precondition(#state{}, tuple()) -> boolean().
precondition(#state{tab=Tab}, {call,_,new,[?TAB,_Options]}) ->
    Tab =:= undefined;
precondition(_S, {call,_,_,_}) ->
    true.

-spec postcondition(#state{}, tuple(), term()) -> boolean().
postcondition(#state{tab=undefined}, {call,_,new,[?TAB,_Options]}, Res) ->
    is_table(Res);
postcondition(_S, {call,_,new,[?TAB,_Options]}, Res) ->
    case Res of
        {'EXIT', {badarg, _}} ->
            true;
        _ ->
            false
    end;
postcondition(_S, {call,_,insert,[_Tab,_ObjOrObjs]}, Res) ->
    Res =:= true;
postcondition(S, {call,_,insert_new,[_Tab,Objs]}, Res) when is_list(Objs) ->
    Res =:= has_insert_new_objs(S, Objs);
postcondition(S, {call,_,insert_new,[_Tab,Obj]}, Res) ->
    Res =:= has_insert_new_objs(S, [Obj]);
postcondition(_S, {call,_,delete,[_Tab]}, Res) ->
    Res =:= true;
postcondition(_S, {call,_,delete,[_Tab,_Key]}, Res) ->
    Res =:= true;
postcondition(_S, {call,_,delete_all_objects,[_Tab]}, Res) ->
    Res =:= true;
postcondition(S, {call,_,lookup,[_Tab,Key]}, Res) ->
    Res =:= keyfind(Key, S);
postcondition(#state{objs=[]}, {call,_,first,[_Tab]}, Res) ->
    Res =:= '$end_of_table';
postcondition(#state{type=set}=S, {call,_,first,[_Tab]}, Res) ->
    keymember(Res, S);
postcondition(#state{type=ordered_set}=S, {call,_,first,[_Tab]}, Res) ->
    #obj{key=K} = hd(sort(S)),
    Res =:= K;
postcondition(#state{type=set}=S, {call,_,next,[_Tab, Key]}, {'EXIT',{badarg,_}}) ->
    not keymember(Key, S);
postcondition(#state{type=set}=S, {call,_,next,[_Tab, Key]}, '$end_of_table') ->
    keymember(Key, S);
postcondition(#state{type=set}=S, {call,_,next,[_Tab, Key]}, Res) ->
    keymember(Key, S) andalso keymember(Res, S);
postcondition(#state{type=ordered_set, objs=[]}, {call,_,next,[_Tab, _Key]}, Res) ->
    Res =:= '$end_of_table';
postcondition(#state{type=ordered_set}=S, {call,_,next,[_Tab, Key]}, Res) ->
    case lists:dropwhile(fun(#obj{key=X}) -> lteq(X, Key, S) end, sort(S)) of
        [] ->
            Res =:= '$end_of_table';
        [#obj{key=K}|_] ->
            Res =:= K
    end;
postcondition(#state{type=set}=S, {call,_,tab2list,[_Tab]}, Res) ->
    [] =:= (S#state.objs -- Res);
postcondition(#state{type=ordered_set}=S, {call,_,tab2list,[_Tab]}, Res) ->
    sort(S) =:= Res;
postcondition(_S, {call,_,_,_}, _Res) ->
    false.

-spec init() -> ok.
init() ->
    ok.

-spec init(#state{}) -> ok.
init(_State) ->
    teardown_table(?TAB),
    ok.

-spec stop(#state{}, #state{}) -> ok.
stop(_State0, _State) ->
    teardown_table(?TAB),
    ok.

-spec aggregate([{integer(), term(), term(), #state{}}])
               -> [{atom(), integer(), term()}].
aggregate(L) ->
    [ {Cmd,length(Args),filter_reply(Reply)} || {_N,{set,_,{call,_,Cmd,Args}},Reply,_State} <- L ].

filter_reply({'EXIT',{Err,_}}) ->
    {error,Err};
filter_reply(_) ->
    ok.


%%%----------------------------------------------------------------------
%%% Internal - Generators
%%%----------------------------------------------------------------------

gen_options(_Op,_S) ->
    [gen_ets_type(), public, named_table, {keypos,#obj.key}].

gen_ets_type() ->
    noshrink(oneof([set, ordered_set])).

gen_integer_key() ->
    oneof(?INT_KEYS).

gen_float_key() ->
    oneof(?FLOAT_KEYS).

gen_binary_key() ->
    oneof(?BINARY_KEYS).

gen_key() ->
    frequency([{5, gen_integer_key()}, {1, gen_float_key()}, {1, gen_binary_key()}]).

gen_key(#state{objs=[]}) ->
    gen_key();
gen_key(#state{objs=Objs}) ->
    oneof([?LET(Obj, oneof(Objs), Obj#obj.key), gen_key()]).

gen_int_or_float_or_bin() ->
    frequency([{5, int()}, {1, real()}, {1, binary()}]).

gen_val() ->
    gen_int_or_float_or_bin().

gen_obj() ->
    #obj{key=gen_key(), val=gen_val()}.

gen_obj(#state{objs=[]}) ->
    gen_obj();
gen_obj(#state{objs=Objs}) ->
    oneof([oneof(Objs), gen_obj()]).

gen_objs(S) ->
    frequency([{9, non_empty(list(gen_obj(S)))}, {1, list(gen_obj(S))}]).


%%%----------------------------------------------------------------------
%%% Internal - Model
%%%----------------------------------------------------------------------

insert_objs(S, []) ->
    S;
insert_objs(S, [#obj{key=K}=Obj|T]) ->
    case keymember(K, S) of
        false ->
            insert_objs(S#state{objs=[Obj|S#state.objs]}, T);
        true ->
            insert_objs(S#state{objs=keyreplace(K, Obj, S)}, T)
    end.

insert_new_objs(S, L) ->
    insert_new_objs(S, lists:reverse(L), S).

insert_new_objs(S, [], _S0) ->
    S;
insert_new_objs(S, [#obj{key=K}=Obj|T], S0) ->
    case keymember(K, S) of
        false ->
            NewT = keydelete(K, T, S),
            insert_new_objs(S#state{objs=[Obj|S#state.objs]}, NewT, S0);
        true ->
            S0
    end.

has_insert_new_objs(S, L) ->
    has_insert_new_objs(S, lists:reverse(L), true).

has_insert_new_objs(_S, [], Bool) ->
    Bool;
has_insert_new_objs(S, [#obj{key=K}=Obj|T], _Bool) ->
    case keymember(K, S) of
        false ->
            NewT = keydelete(K, T, S),
            has_insert_new_objs(S#state{objs=[Obj|S#state.objs]}, NewT, true);
        true ->
            false
    end.

keydelete(X, #state{objs=L}=S) ->
    keydelete(X, L, S).

keydelete(X, L, S) ->
    lists:filter(fun(#obj{key=K}) -> neq(X, K, S) end, L).

keyreplace(X, Y, #state{objs=L}=S) ->
    lists:map(fun(Z=#obj{key=K}) -> case eq(X, K, S) of true -> Y; false -> Z end end, L).

keyfind(X, #state{objs=L}=S) ->
    lists:filter(fun(#obj{key=K}) -> eq(X, K, S) end, L).

keymember(X, S) ->
    [] =/= keyfind(X, S).

eq(X, Y, #state{type=set}) ->
    X =:= Y;
eq(X, Y, #state{type=ordered_set}) ->
    X == Y.

neq(X, Y, #state{type=set}) ->
    X =/= Y;
neq(X, Y, #state{type=ordered_set}) ->
    X /= Y.

lteq(X, Y, _S) ->
    X =< Y.

sort(#state{objs=L}) ->
    lists:sort(L).

%%%----------------------------------------------------------------------
%%% Internal - Implementation
%%%----------------------------------------------------------------------

teardown_table(Tab) ->
    catch ets:delete(Tab).

is_table(Tab) ->
    is_atom(Tab).

new(Tab, Options) ->
    catch ets:new(Tab, Options).

insert(Tab, ObjOrObjs) ->
    catch ets:insert(Tab, ObjOrObjs).

insert_new(Tab, ObjOrObjs) ->
    catch ets:insert_new(Tab, ObjOrObjs).

delete(Tab) ->
    catch ets:delete(Tab).

delete(Tab, Key) ->
    catch ets:delete(Tab, Key).

delete_all_objects(Tab) ->
    catch ets:delete_all_objects(Tab).

lookup(Tab, Key) ->
    catch ets:lookup(Tab, Key).

first(Tab) ->
    catch ets:first(Tab).

next(Tab, Key) ->
    catch ets:next(Tab, Key).

info(Tab, Item) ->
    catch ets:info(Tab, Item).

tab2list(Tab) ->
    catch ets:tab2list(Tab).

-endif. %% -ifdef(QC_STATEM).

-endif. %% -ifdef(QC).
