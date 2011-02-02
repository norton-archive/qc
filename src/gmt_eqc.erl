%%%-------------------------------------------------------------------
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
%%% File    : gmt_eqc.erl
%%% Purpose : Wrapper for eqc.erl
%%%-------------------------------------------------------------------

-module(gmt_eqc).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").

-export([start/0]).

%% @doc Starts (and possibly restarts) the QuickCheck server. If
%% another instance is not running, start the server and return the
%% server's process id.  If another instance is already running,
%% return true.  Otherwise, forcefully restart the server.
start() ->
    case (catch eqc:start(false)) of
        {'EXIT', already_running} ->
            true;
        Pid when is_pid(Pid) ->
            Pid;
        _ ->
            eqc:start(true)
    end.

-endif. %% -ifdef(EQC).
