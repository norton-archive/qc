%%%----------------------------------------------------------------------
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
%%% File    : gmt_genutil.erl
%%% Purpose : GMT gen utilities
%%%----------------------------------------------------------------------

-module(gmt_genutil).

-export([
         quick_global_server_call/2,
         quick_global_server_call/3,
         server_call/2,
         server_call/3,
         fsm_sync_send_event/2,
         fsm_sync_send_event/3,
         fsm_sync_send_all_state_event/2,
         fsm_sync_send_all_state_event/3,
         fsm_send_event/2,
         fsm_send_all_state_event/2,
         quick_global_call/2,
         quick_global_call/3,
         catchexit/1
        ]).

%% @spec (ServerRef, Request) -> Reply | timeout | retrylater
%% @doc gen_server quick_global_call with catchexit replies

quick_global_server_call(ServerRef, Request) ->
    quick_global_server_call(ServerRef, Request, infinity).

quick_global_server_call(ServerRef, Request, Timeout) ->
    catchexit(catch quick_global_call(ServerRef, Request, Timeout)).

%% @spec (ServerRef, Request) -> Reply | timeout | retrylater
%% @doc gen_server server_call with catchexit replies

server_call(ServerRef, Request) ->
    server_call(ServerRef, Request, infinity).

server_call(ServerRef, Request, Timeout) ->
    catchexit(catch gen_server:call(ServerRef, Request, Timeout)).

%% @spec (FSMRef, Event) -> Reply | timeout | retrylater
%% @doc gen_fsm sync_send_event with catchexit replies

fsm_sync_send_event(FSMRef, Event) ->
    fsm_sync_send_event(FSMRef, Event, infinity).

fsm_sync_send_event(FSMRef, Event, Timeout) ->
    catchexit(catch gen_fsm:sync_send_event(FSMRef, Event, Timeout)).

%% @spec (FSMRef, Event) -> Reply | timeout | retrylater
%% @doc gen_fsm sync_send_all_state_event with catchexit replies

fsm_sync_send_all_state_event(FSMRef, Event) ->
    fsm_sync_send_all_state_event(FSMRef, Event, infinity).

fsm_sync_send_all_state_event(FSMRef, Event, Timeout) ->
    catchexit(catch gen_fsm:sync_send_all_state_event(FSMRef, Event, Timeout)).

%% @spec (FSMRef, Event) -> ok
%% @doc gen_fsm send_event

fsm_send_event(FSMRef, Event) ->
    gen_fsm:send_event(FSMRef, Event).

%% @spec (FSMRef, Event) -> ok
%% @doc gen_fsm send_all_state_event

fsm_send_all_state_event(FSMRef, Event) ->
    gen_fsm:send_all_state_event(FSMRef, Event).

%% @spec ({global, term()}, term()) -> term()
%% @equiv quick_global_call({global, Name}, Arg, 5000)

quick_global_call({global, Name}, Arg) ->
    quick_global_call({global, Name}, Arg, 5000).

%% @spec ({global, term()}, term(), integer() | infinity) -> term()
%% @doc Same usage as <tt>gen_server:call({global, Name}, Args)</tt> but
%% without the "infrequent but sometimes 0.25 - 2.5 second call latency"
%% problem that gen_server:call() has.
%%
%% AFAICT, the problem is gen.erl's usage of global:safe_whereis_name/1.

quick_global_call({global, Name}, Arg, Timeout) ->
    case global:whereis_name(Name) of
        undefined ->
            exit(noproc);
        Pid ->
            gen_server:call(Pid, Arg, Timeout)
    end.

%% @spec (Reply) -> Reply | timeout | retrylater
%% @doc Catch EXIT replies

catchexit(Reply) ->
    case Reply of
        {'EXIT', {timeout, _}} ->
            timeout;
        {'EXIT', {_, _}} ->
            retrylater;
        Reply ->
            Reply
    end.
