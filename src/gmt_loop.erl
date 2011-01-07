%%%-------------------------------------------------------------------
%%% Copyright (c) 2007-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_loop.erl
%%% Purpose : A set of looping, iteration, and timing functions
%%%-------------------------------------------------------------------

%%% @doc A set of looping, iteration, and timing convenience functions
%%%
%%% In many situations, Erlang list comprehensions can be (ab)used to
%%% for simple iterative/looping style constructs.  The OTP stdlib
%%% <tt>lists</tt> module provides a few useful functions ... but there
%%% are situations where neither provides the utility that you
%%% <em>really</em> want or need.  The functions in this module attempt
%%% to fill in that gap.

-module(gmt_loop).

-export([do_while/2,
         tc/1, tc_iter/2, tc_iter/4]).


%% TODO: Incomplete!  100% of public API is not yet covered.

-spec(do_while/2 :: (fun((_) -> any()),_) -> any() ).


%% @spec (fun(), term()) -> term()
%% @doc Mimic a "do...while" loop, executing <tt>Fun</tt> at least once.
%%
%% The anonymous function <tt>Fun/1</tt> must return a 2-tuple:
%% <ol>
%% <li> <tt>true | false</tt> </li>
%% <li> A new accumulator, to be passed to the next <tt>Fun/1</tt> call. </li>
%% </ol>
%%
%% Example usage:
%% <pre>
%% F = fun({Table, Key}) ->
%%          case foo_database:lookup(Table, Key) of
%%              bar                 -> {true, {Table, Key}};
%%              {baz, DesiredValue} -> {false, DesiredValue}
%%          end
%%     end,
%% gmt_loop(F, {some_table, some_key}).
%% </pre>

do_while(Fun, InitialAcc) ->
    do_while2(Fun(InitialAcc), Fun).

do_while2({true, Acc}, Fun) ->
    do_while2(Fun(Acc), Fun);
do_while2({false, Acc}, _Fun) ->
    Acc.

%% @spec (fun()) -> {integer(), term()}
%% @doc Like <tt>timer:tc/3</tt> but for a fun of arity 0.

tc(Fun) ->
    Start = now(),
    Res = Fun(),
    End = now(),
    {timer:now_diff(End, Start), Res}.

%% @spec (fun(), integer()) -> {integer(), integer(), term()}
%% @doc Like <tt>timer:tc/3</tt> but for a fun of arity 0, iterating
%% several times, returning the sum of elapsed time, average elapsed
%% time, and first iteration's result.

tc_iter(Fun, Iters) when Iters > 0 ->
    Ts = [tc(Fun) || _ <- lists:seq(1, Iters)],
    Sum = lists:foldl(fun({Time, _}, Acc) -> Acc + Time end, 0, Ts),
    {Sum, round(Sum / Iters), element(2, hd(Ts))}.

%% @spec (atom(), atom(), list(), integer()) -> {integer(), integer(), term()}
%% @doc Like <tt>timer:tc/3</tt>, iterating several times, returning
%% the sum of elapsed time, average elapsed time, and first iteration's result.

tc_iter(M, F, A, Iters) when Iters > 0 ->
    Ts = [timer:tc(M, F, A) || _ <- lists:seq(1, Iters)],
    Sum = lists:foldl(fun({Time, _}, Acc) -> Acc + Time end, 0, Ts),
    {Sum, round(Sum / Iters), element(2, hd(Ts))}.

