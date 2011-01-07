%%%----------------------------------------------------------------------
%%% Copyright (c) 2006-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File     : gmt_util_app.erl
%%% Purpose  : GMT util application callback module
%%%----------------------------------------------------------------------

-module(gmt_util_app).

-behaviour(application).

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
    start(normal, []).

start(_Type, StartArgs) ->
    catch gmt_cinfo_basic:register(),
    case gmt_util_sup:start_link(StartArgs) of
        {ok, _Pid} = Ok ->
            Ok;
        Error ->
            Error
    end.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
stop(_State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
