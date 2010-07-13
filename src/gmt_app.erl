%%%----------------------------------------------------------------------
%%% Copyright: (c) 2006-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File     : gmt_app.erl
%%% Purpose  : GMT dummy application callback module
%%%----------------------------------------------------------------------

-module(gmt_app).

-behaviour(application).

-include("applog.hrl").

%% application callbacks
-export([start/0, start/2, stop/1]).

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%%----------------------------------------------------------------------
start() ->
    start(xxxwhocares, []).

start(Type, StartArgs) ->
    gmt_event_h:start_singleton_report_handler(?MODULE, Type, StartArgs),
    catch gmt_cinfo_basic:register(),
    case gmt_sup:start_link(StartArgs) of
        {ok, Pid} ->
            io:format("QQQ: ~s:start ok Pid = ~p\n", [?MODULE, Pid]),
            {ok, Pid};
        Error ->
            io:format("QQQ: ~s:start bummer: ~p\n", [?MODULE, Error]),
            Error
    end.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
stop(State) ->
    ?APPLOG_DEBUG("stop: ~p", [State]),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
