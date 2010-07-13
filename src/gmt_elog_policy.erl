%%%----------------------------------------------------------------------
%%% Copyright: (c) 2009-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_elog_policy.erl
%%% Purpose : GMT event logging policy module
%%%----------------------------------------------------------------------

%%%
%%% @doc A lightweight event logging policy implementation.
%%%
%%% The emphasis of this module is <em>lightweight</em>.  The enabled()
%%% function may be called tens of thousands of times per second.
%%%
%%% The use of the 4 arguments to enabled can be free-form, since
%%% this entire module will be replaced whenever tracing is desired.
%%% However, it's strongly recommended that the following conventions
%%% be used:
%%%
%%% <ul>
%%% <li> <b>Priority</b>: integer() between ?LOG_EMERG_PRI (0) and
%%%      ?LOG_DEBUG_PRI (7). </li>
%%% <li> <b>Category</b>: atom() </li>
%%% <li> <b>Module</b>: atom() </li>
%%% <li> <b>Line</b>: integer() </li>
%%% </ul>
%%%

-module(gmt_elog_policy).

-include("gmt_applog.hrl").

-export([enabled/6]).
-export([test_c/6, test_e/6, test_e_setup/0]).


-spec(enabled/6 :: (term(), term(), atom(), integer(), string(), list()) ->
             'true' | 'false' ).


enabled(Priority, _Category, _Module, _Line, _Fmt, _ArgList)
  when Priority =< ?LOG_INFO_PRI ->
    true;
enabled(_Priority, _Category, _Module, _Line, _Fmt, _ArgList) ->
    false.

%% Functions for experimentation only.

test_c(_Priority, _Category, _Module, _Line, _Fmt, _ArgList) ->
    false.

%% My laptop, CPU clock fixed at 1.33GHz, non-SMP VM, says:
%%
%% timer:tc(gmt_elog, test_iter, [0, 88999000])    -> {12925467,ok}
%% timer:tc(gmt_elog, test_iter_c, [0, 88999000])  -> {10012760,ok}
%% timer:tc(gmt_elog, test_iter_e, [0, 88999000])  -> {43373826,ok}
%%
%% The enabled() func is 3.4x faster than test_e() when test_e()'s
%% public table exists and contains the single tuple {test_e, 5}.
%%
%% If the public named table does not exist, test_e() is 25.6x slower
%% than enabled()!
%% If the public named table exists and is empty, test_e() is 2.2x slower
%% than enabled().

test_e(Priority, _Category, _Module, _Line, _Fmt, _ArgList) ->
    case (catch ets:lookup(goofus, test_e)) of
        [{test_e, Limit}] ->
            Priority =< Limit;
        _ ->
            false
    end.

test_e_setup() ->
    spawn(fun() ->
                  ets:new(goofus, [public, named_table]),
                  receive goofus -> goofus end
          end).
