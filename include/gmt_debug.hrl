%%%----------------------------------------------------------------------
%%% Copyright (c) 2007-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_debug.erl
%%% Purpose : Conditional macros for debugging assistance/convenience, etc.
%%%----------------------------------------------------------------------

-ifndef('gmt_debug.hrl').
-define('gmt_debug.hrl', true).

-ifdef(gmt_debug).

-define(FUNCTION,element(2,process_info(self(),current_function))).
-define(HERE, {?MODULE, ?LINE, ?FUNCTION}).

%% Print the string-ified version of an expression *and* its value.
-define(DBG(EXPR), io:format("DEBUG(~w): " ++ ??EXPR ++ " = ~p\n", [?HERE, EXPR])).

%% Print the string-ified version of an expression *and* its value,
%% using "~p" instead of "~p".
-define(DBGW(EXPR), io:format("DEBUG(~w): " ++ ??EXPR ++ " = ~p\n", [?HERE, EXPR])).

%% Convenient way to call io:format/2.
-define(DBGF(FMT, ARGS), io:format(io_lib:format("DEBUG(~w): ", [?HERE]) ++ FMT ++ "\n", ARGS)).

-else.  % gmt_debug

-define(DBG(EXPR),     do_nothing).
-define(DBGW(EXPR),    do_nothing).
-define(DBGF(FMT, ARGS), do_nothing).

-endif. % gmt_debug

-endif. % 'gmt_debug.hrl'
