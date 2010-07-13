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
%%% File    : gmt_elog.erl
%%% Purpose : GMT event logging
%%%----------------------------------------------------------------------

-module(gmt_elog).

-include("gmt_elog.hrl").

%% API

-export([help/0,
         start_tracing/0, start_tracing/1, stop_tracing/0,
         add_match_spec/0, add_match_spec/1, del_match_spec/0,
         format_file/1, format_file/2, format_file/3]).

%% Micro benchmarking

-export([test_iter/2]).
-export([test_iter_e/2]).
-export([test_iter_c/2]).

%%
%% Public API
%%

help() ->
  io:format("~s", [
    "  gmt_elog:start_tracing().\n"
    "  gmt_elog:start_tracing(\"/path/to/trace-file\").\n"
    "  gmt_elog:add_match_spec().         ... to trace everything\n"
    "  gmt_elog:add_match_spec(dbg:fun2ms(fun([_, target_category, _, _, _, _]) -> return_trace() end)).\n"
    "  gmt_elog:add_match_spec(dbg:fun2ms(fun([Pri, _, _, _, _, _]) when Prio > 0 -> return_trace() end)).\n"
    "  gmt_elog:add_match_spec(dbg:fun2ms(fun([_, target_category, target_module, _, _, _]) -> return_trace(); ([_, _, other_module, 88, _, _]) -> return_trace() end)).\n"
    "  gmt_elog:del_match_spec().\n"
    "  gmt_elog:stop_tracing().\n"
    "  gmt_elog:format_file(\"/path/to/trace-file\").\n"
    "  gmt_elog:format_file(\"/path/to/trace-file\", \"/path/to/output\").\n"
    "  gmt_elog:format_file(\"/path/to/trace-file\", \"/path/to/output\", MatchStr|\"\").\n"
  ]).

%% @spec () -> ok
%% @doc Start tracing, using the default tracer to the CLI console
%%      (for interactive/dev use only).

start_tracing() ->
    {ok,_} = dbg:tracer(),
    start_tracing_bottom().

%% @spec (string()) -> ok
%% @doc Start tracing, dumping raw trace events into TraceFileName.

start_tracing(TraceFileName) ->
    {ok,_} = dbg:tracer(port, dbg:trace_port(file, TraceFileName)),
    start_tracing_bottom().

start_tracing_bottom() ->
    dbg:p(all, [call, timestamp]),
    ok.

%% @spec () -> ok
%% @doc Stop tracing that started with start_tracing/1 and closing
%%      the trace file.

stop_tracing() ->
    dbg:stop().

%% @doc Trace all gmt_elog events.

add_match_spec() ->
  io:format("~s", [
    "Please cut-and-paste this function to your shell:\n"
    "\n"
    "  gmt_elog:add_match_spec(dbg:fun2ms(fun([_, _, _, _, _, _]) -> return_trace() end)).\n"
  ]).

%% @spec (dbg_match_spec()) -> ok
%% @doc Add the dbg:fun2ms() style match spec to the tracing system.
%%
%% Examples:
%%
%% <ul>
%% <li> <tt> add_match_spec(dbg:fun2ms(fun([_, target_category, _, _, _, _]) -> return_trace() end))  </tt> </li>
%% <li> <tt> add_match_spec(dbg:fun2ms(fun([Pri, _, _, _, _, _]) when Prio > 0 -> return_trace() end))  </tt> </li>
%% <li> <tt> add_match_spec(dbg:fun2ms(fun([_, target_category, target_module, _, _, _]) -> return_trace(); ([_, _, other_module, 88, _, _]) -> return_trace() end))  </tt> </li>
%% </ul>

add_match_spec(MatchSpec) ->
    dbg:tpl(gmt_elog_policy, enabled, MatchSpec),
    ok.

%% @spec () -> ok
%% @doc Delete the tracing match spec from the tracing subsystem, keeping
%% the trace file open for later tracing.

del_match_spec() ->
    dbg:ctpl(gmt_elog_policy, enabled),
    ok.

%% @spec (string()) -> ok
%% @doc Format a raw trace file into a human-readable file.

format_file(TraceFileName) ->
    {ok, _} = file:read_file_info(TraceFileName), % Sanity
    dbg:trace_client(file, TraceFileName, {fun fmt/2, {user, "", 0, ""}}).

format_file(TraceFileName, OutputFile) ->
    format_file(TraceFileName, OutputFile, "").

%% @spec (string(), string(), [] | string()) -> ok
%% @doc Format a raw trace file into a human-readable file.
%%
%% If the MatchStr argument is "", all trace entries will be output.
%% If MatchStr is any other string, then only trace entries which
%% contain that string (exact string match, case sensitive, definitely
%% no regexps!) will be output.

format_file(TraceFileName, OutputFile, MatchStr) ->
    {ok, _} = file:read_file_info(TraceFileName), % Sanity
    dbg:trace_client(file, TraceFileName,
                     {fun fmt/2, {undefined, OutputFile, 0, MatchStr}}).

fmt(end_of_trace, Acc) ->
    io:format("End of trace reached, Acc = ~p\n", [Acc]),
    Acc;
fmt({trace_ts, _Pid, call,
     {_Mod, _Fun, [Pri, Cat, Module, Line, Fmt0, ArgList]} = _MFA, TS} = _X,
    {OutputDev0, OutputFile, Sum, MatchStr}) ->
    %% This is really weird: passing a file:open(Path, [write]) file
    %% handle into this spawn'ed trace client proc doesn't work!
    %% Grr!!!
    OutputDev = if OutputDev0 == undefined ->
                        {ok, FH} = file:open(OutputFile, [write]),
                        io:format("trace client: opened new FH ~p\n", [FH]),
                        FH;
                   true ->
                        OutputDev0
                end,
    Fmt = if Fmt0 == "~p" -> "~250p";
             true         -> Fmt0
          end,
    Bs = format_1(TS, Pri, Cat, Module, Line, Fmt, ArgList),
    Match = case MatchStr == "" orelse string:str(Bs, MatchStr) > 0 of
                true ->
                    io:put_chars(OutputDev, Bs),
                    1;
                false ->
                    0
            end,
    {OutputDev, OutputFile, Sum + Match, MatchStr};
fmt({trace, _Pid, call,
     {_Mod, _Fun, [Pri, Cat, Module, Line, Fmt0, ArgList]} = _MFA} = _X,
    {OutputDev0, OutputFile, Sum, MatchStr}) ->
    OutputDev = if OutputDev0 == undefined ->
                        {ok, FH} = file:open(OutputFile, [write]),
                        io:format("trace client: opened new FH ~p\n", [FH]),
                        FH;
                   true ->
                        OutputDev0
                end,
    Fmt = if Fmt0 == "~p" -> "~250p";
             true         -> Fmt0
          end,
    Bs = format_1({0,0,0}, Pri, Cat, Module, Line, Fmt, ArgList),
    Match = case MatchStr == "" orelse string:str(Bs, MatchStr) > 0 of
                true ->
                    io:put_chars(OutputDev, Bs),
                    1;
                false ->
                    0
            end,
    {OutputDev, OutputFile, Sum + Match, MatchStr};
fmt({trace_ts, _Pid, return_from, _MFA, _Hrm, _TS}, Acc) ->
    %% Yes, we're ignoring half of our hard-won the tracing data.
    Acc;
fmt({trace, _Pid, return_from, _MFA, _Hrm}, Acc) ->
    %% Yes, we're ignoring half of our hard-won the tracing data.
    Acc;
fmt(_X, Acc) ->
    io:format("Clause 4, _X = ~p, Acc = ~p\n", [_X, Acc]),
    Acc.

format_1(TS, Pri, Cat, Module, Line, Fmt, ArgList) when is_list(Fmt) ->
    lists:flatten(
      io_lib:format("~p|~p|~p|~p|~p|" ++ Fmt ++ "\n",
                    [calendar:now_to_local_time(TS), Pri, Cat, Module, Line]
                    ++ ArgList));
format_1(_TS, _Pri, _Cat, _Module, _Line, _Fmt, _ArgList) ->
    "".

%%
%% Micro benchmark functions, use with timer:tc/3.
%%

test_iter(_Priority, 0) ->
    ok;
test_iter(Priority, N) ->
    gmt_elog_policy:enabled(Priority, foo, ?MODULE, ?LINE, "Hello, ~s", ["world!"]),
    test_iter(Priority, N - 1).

test_iter_e(_Priority, 0) ->
    ok;
test_iter_e(Priority, N) ->
    gmt_elog_policy:test_e(Priority, foo, ?MODULE, ?LINE, "Hello, ~s", ["world!"]),
    test_iter_e(Priority, N - 1).

test_iter_c(_Priority, 0) ->
    ok;
test_iter_c(Priority, N) ->
    gmt_elog_policy:test_c(Priority, foo, ?FILE, ?LINE, "Hello, ~s", ["world!"]),
    test_iter_c(Priority, N - 1).

