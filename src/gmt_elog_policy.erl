%%%----------------------------------------------------------------------
%%% Copyright (c) 2009-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% The emphasis of this module is <em>lightweight</em>.  The
%%% enabled() function may be called tens of thousands of times per
%%% second.
%%%
%%% The use of the 4 arguments to enabled can be free-form, since this
%%% entire module will be replaced whenever tracing is desired.
%%% However, it's strongly recommended that the following conventions
%%% be used:
%%%
%%% <ul>
%%% <li> <b>Level</b>: error | warning | info | debug </li>
%%% <li> <b>Category</b>: term() </li>
%%% <li> <b>Module</b>: module() </li>
%%% <li> <b>Line</b>: integer() </li>
%%% </ul>
%%%

-module(gmt_elog_policy).

%% API
-export([enabled/6]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Fixture for event logging and event tracing. Returns true if
%% event was reported to event_logger.  Otherwise, returns false.
%%--------------------------------------------------------------------

-type log_level() :: 'error' | 'warning' | 'info' | 'debug'.

-spec enabled(log_level(), term(), module(), integer(), string(), list())
             -> 'true' | 'false'.

enabled(error, _Category, Module, Line, Fmt, Args) ->
    case application:get_env(gmt_util, application_evt_log_level) of
        {ok, Level} when Level == info; Level == warning; Level == error ->
            Report = report(Module, Line, Fmt, Args),
            case application:get_env(gmt_util, application_evt_log_type) of
                {ok, undefined} ->
                    ok = error_logger:error_report(Report);
                {ok, Type} ->
                    ok = error_logger:error_report(Type, Report)
            end,
            true;
        {ok, _Level} ->
            false
    end;
enabled(warning, _Category, Module, Line, Fmt, Args) ->
    case application:get_env(gmt_util, application_evt_log_level) of
        {ok, Level} when Level == info; Level == warning ->
            Report = report(Module, Line, Fmt, Args),
            case application:get_env(gmt_util, application_evt_log_type) of
                {ok, undefined} ->
                    ok = error_logger:warning_report(Report);
                {ok, Type} ->
                    ok = error_logger:warning_report(Type, Report)
            end,
            true;
        {ok, _Level} ->
            false
    end;
enabled(info, _Category, Module, Line, Fmt, Args) ->
    case application:get_env(gmt_util, application_evt_log_level) of
        {ok, Level} when Level == info ->
            Report = report(Module, Line, Fmt, Args),
            case application:get_env(gmt_util, application_evt_log_type) of
                {ok, undefined} ->
                    ok = error_logger:info_report(Report);
                {ok, Type} ->
                    ok = error_logger:info_report(Type, Report)
            end,
            true;
        {ok, _Level} ->
            false
    end;
enabled(_Level, _Category, _Module, _Line, _Fmt, _Args) ->
    false.

%%%===================================================================
%%% Internal functions
%%%===================================================================

report(Module, Line, Fmt, Args) ->
    Msg = lists:flatten(io_lib:format(Fmt, Args)),
    [{module, Module}, {line, Line}, {msg, Msg}].
