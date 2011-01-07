%%%-------------------------------------------------------------------
%%% Copyright (c) 2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_parallel_limit.erl
%%% Purpose : Parallel activity limiting server
%%%-------------------------------------------------------------------

-module(gmt_parallel_limit).

-behaviour(gen_server).

-include("gmt_elog.hrl").

%% API
-export([start_link/1, start_link/2, enqueue/2, get_limit/1, change_limit/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% For simple code, don't keep track of individual spawned pids: if
%% we get a 'DOWN' message, we assume it's because we spawned it.
%% If someone sense us fake 'DOWN' messages, the world won't end if
%% the current count gets skewed.  If the world would indeed end, then
%% you need to use another mechanism.

-record(state, {
          max,                                  % integer
          current,                              % integer
          q                                     % queue()
         }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(MaxNum) ->
    gen_server:start_link(?MODULE, [MaxNum], []).

start_link(RegName, MaxNum) ->
    gen_server:start_link({local, RegName}, ?MODULE, [MaxNum], []).

%% @spec (server_ref(), fun_arity_0()) -> ok | queued

enqueue(ServerRef, FunArity0) ->
    gen_server:call(ServerRef, {enqueue, FunArity0}).

%% @spec (server_ref()) -> {integer(), integer()}
%% @doc Return numbers of currently executing procs and maximum limit.

get_limit(ServerRef) ->
    gen_server:call(ServerRef, {get_limit}).

%% @spec (server_ref(), integer()) -> ok
%% @doc Change the maximum limit for `ServerRef'.

change_limit(ServerRef, Max) ->
    gen_server:call(ServerRef, {change_limit, Max}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([MaxNum]) ->
    {ok, #state{max = MaxNum, current = 0, q = queue:new()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({enqueue, Fun}, _From, #state{max = Max, current = Current}=State)->
    if Current < Max ->
            _ = spawn_monitor(Fun),
            {reply, ok, State#state{current = Current + 1}};
       true ->
            {reply, queued, State#state{q = queue:in(Fun, State#state.q)}}
    end;
handle_call({get_limit}, _From, State) ->
    {reply, {State#state.current, State#state.max}, State};
handle_call({change_limit, Max}, _From, State) ->
    {reply, ok, State#state{max = Max}};
handle_call(_Request, _From, State) ->
    Reply = 'wha???',
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', _, _, _, _},
            #state{max = Max, current = Current, q = Q} = State) ->
    if Max == Current ->
            case queue:out(Q) of
                {empty, _} ->
                    {noreply, State#state{current = Current - 1}};
                {{value, Fun}, Q2} ->
                    _ = spawn_monitor(Fun),
                    {noreply, State#state{q = Q2}}
            end;
       true ->
            {noreply, State#state{current = Current - 1}}
    end;
handle_info(_Info, State) ->
    ?ELOG_INFO("handle_info: ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
