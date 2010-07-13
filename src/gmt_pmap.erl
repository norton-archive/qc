%%%----------------------------------------------------------------------
%%% Copyright (c) 2009-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_pmap.erl
%%% Purpose : GMT PMap
%%%----------------------------------------------------------------------

-module(gmt_pmap).

-export([pmap/2
         , pmap/3
         , pmap/4
         , pmap_kv/2
         , pmap_kv/3
         , pmap_kv/4
        ]).

pmap(Fun, List) ->
    pmap(Fun, List, length(List)).

pmap(Fun, List, Limit) ->
    pmap(Fun, List, Limit, infinity).

%% @spec (function(), list(), integer(), timeout()) ->
%%       {list(), integer(), integer(), integer()}
%% @doc Parallel pmap() (see Erlang/OTP docs for lists:pmap/2).
%%
%% Evaluates Fun(Elem), for every element Elem in List, in parallel
%% subject to the specified Limit number of parallel processes.
%% Returns list of return values, in the same order as in List.  Each
%% result in the first tuple member appears as:
%% <ul>
%% <li> {ok, Res} for successful calls </li>
%% <li> {error, Res} for error calls </li>
%% <li> 'timeout' for timed-out calls </li>
%% </ul>
%%
%% For the 2nd-4th elements of the return tuple, the integers are:
%% number of ok/successful calls, number of error calls, and number of
%% timeout calls.
%% .

pmap(Fun, List, Limit, Timeout)
  when is_integer(Timeout) ->
    Expires = gmt_time:make_expires(Timeout),
    pmap(Fun, List, Limit, Expires);
pmap(Fun, List, Limit, Expires)
  when is_function(Fun, 1), is_list(List), is_integer(Limit), Limit > 0 ->
    pmap(Fun, List, Limit, 0, [], [], 0, 0, 0, Expires);
pmap(Fun, List, 0, Expires) ->
    pmap(Fun, List, 1, Expires).

pmap_kv(Fun, List) ->
    pmap_kv(Fun, List, length(List)).

pmap_kv(Fun, List, Limit) ->
    pmap_kv(Fun, List, Limit, infinity).

%% @spec (function(), list(), integer(), timeout()) ->
%%       {list(), list(), list()}
%% @doc Variation of parallel pmap() (see Erlang/OTP docs for lists:pmap/2).
%%
%% The return value of this variation of pmap is a 3-tuple:
%% <ul>
%% <li> list({Argument, SuccessfulResult}) </li>
%% <li> list({Argument, ErrorResult}) </li>
%% <li> list({Argument, timeout}) </li>
%% </ul>
%%
%% For each of these three lists, the order of the results is the same
%% relative to the original List argument.

pmap_kv(Fun, List, Limit, Timeout) ->
    {Rs, _, _, _} = pmap(Fun, List, Limit, Timeout),
    {G, B, T} =lists:foldl(
                 fun({Arg, {ok, Res}}, {Good, Bad, TimeOut}) ->
                         {[{Arg, Res}|Good], Bad, TimeOut};
                    ({Arg, {error, Res}}, {Good, Bad, TimeOut}) ->
                         {Good, [{Arg, Res}|Bad], TimeOut};
                    ({_, timeout} = X, {Good, Bad, TimeOut}) ->
                         {Good, [X|Bad], TimeOut}
                 end, {[], [], []}, lists:zip(List, Rs)),
    {lists:reverse(G), lists:reverse(B), lists:reverse(T)}.

pmap(Fun, [H|T], Limit, Num, PidRefs, Acc, NumOk, NumErr, NumTo, Expires) when Num < Limit ->
    PidRef = erlang:spawn_monitor(
               fun() ->
                       case (catch Fun(H)) of
                           {'EXIT', Err} ->
                               exit({done_err, {H, Err}});
                           Res ->
                               exit({done_ok, Res})
                       end
               end),
    PidRefs1 = lists:reverse([PidRef|lists:reverse(PidRefs)]),
    pmap(Fun, T, Limit, Num+1, PidRefs1, Acc, NumOk, NumErr, NumTo, Expires);
pmap(_Fun, [], _Limit, _Num, [], Acc, NumOk, NumErr, NumTo, _Expires) ->
    {lists:reverse(Acc), NumOk, NumErr, NumTo};
pmap(Fun, List, Limit, Num, [{Pid, Ref} = _PidRef|PidRefs], Acc, NumOk, NumErr, NumTo, Expires) ->
    Timeout = gmt_time:make_timeout(Expires),
    receive
        {'DOWN', Ref, process, Pid, {done_ok, Res}} ->
            Reply = {ok, Res},
            pmap(Fun, List, Limit, Num-1, PidRefs, [Reply|Acc], NumOk+1, NumErr, NumTo, Expires);
        {'DOWN', Ref, process, Pid, {_, _, {done_ok, Res}}} -> % smart ex.
            Reply = {ok, Res},
            pmap(Fun, List, Limit, Num-1, PidRefs, [Reply|Acc], NumOk+1, NumErr, NumTo, Expires);
        {'DOWN', Ref, process, Pid, {done_err, Err}} ->
            Reply = {error, Err},
            pmap(Fun, List, Limit, Num-1, PidRefs, [Reply|Acc], NumOk, NumErr+1, NumTo, Expires);
        {'DOWN', Ref, process, Pid, {_, _, {done_err, Err}}} -> % smart ex.
            Reply = {error, Err},
            pmap(Fun, List, Limit, Num-1, PidRefs, [Reply|Acc], NumOk, NumErr+1, NumTo, Expires)
    after Timeout ->
            exit(Pid),
            Reply = timeout,
            pmap(Fun, List, Limit, Num-1, PidRefs, [Reply|Acc], NumOk, NumErr, NumTo+1, Expires)
    end.
