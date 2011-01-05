%%%----------------------------------------------------------------------
%%% Copyright: (c) 2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_sysmon_server.erl
%%% Purpose : System monitor
%%%----------------------------------------------------------------------

-module(gmt_sysmon_server).

-behaviour(gen_server).

-include("gmt_elog.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          tref,
          dict,
          count = 0,
          max_per_sec
         }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    %% Use fullsweep_after option to help clean up garbage regularly
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{spawn_opt, [{fullsweep_after, 5}]}]).

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
init([]) ->
    {ok, BigGC} = application:get_env(gmt, sysmon_long_gc),
    {ok, BigHeap} = application:get_env(gmt, sysmon_large_heap),
    {ok, MaxPS} = application:get_env(gmt, sysmon_max_per_second),
    _ = erlang:system_monitor(self(), [{long_gc, BigGC}, {large_heap, BigHeap},
                                       busy_port, busy_dist_port]),
    {ok, TRef} = timer:send_interval(1000, reset_dict),
    {ok, #state{tref = TRef, dict = orddict:new(), max_per_sec = MaxPS}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
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
handle_info({monitor, _Pid, _Type, _Info}, #state{count = Count} = State) when Count > State#state.max_per_sec ->
    {noreply, State#state{count = Count + 1}};
handle_info({monitor, Pid, Type, _Info} = Msg, #state{dict = Dict, count = Count} = State) ->
    FiltMsg = if Type =:= large_heap orelse Type =:= long_gc ->
                  {monitor, Pid, Type, info_filtered};
              true ->
                  Msg
              end,
    try
        NewDict = orddict:update(FiltMsg, fun(Val) -> Val+1 end, Dict),
        {noreply, State#state{dict = NewDict, count = Count + 1}}
    catch _X:_Y ->
        NewDict2 = orddict:store(FiltMsg, 1, Dict),
        PInfo = get_pretty_info(Pid),
        ?ELOG_INFO("~P pinfo: ~p\n", [Msg, 30, PInfo]),
        {noreply, State#state{dict = NewDict2, count = Count + 1}}
    end;
handle_info(reset_dict, #state{dict = Dict, count = Count} = State) ->
    if Count > State#state.max_per_sec ->
        ?ELOG_INFO("~p messages suppressed\n", [Count - State#state.max_per_sec]);
    true ->
        ok
    end,
    _ = [?ELOG_INFO("~p extra: ~p\n", [N-1, Msg]) || {Msg, N} <- orddict:to_list(Dict), N > 1],
    {noreply, State#state{dict = orddict:new(), count = 0}};
handle_info(_Info, State) ->
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

get_pretty_info(Pid) ->
    case process_info(Pid, [registered_name, initial_call, current_function]) of
        undefined ->
            undefined;
        [] ->
            undefined;
        [{registered_name,[]},IC,CF] ->
            [IC,CF];
        [RN,_IC,CF]=Props ->
            case proc_lib:initial_call(Pid) of
                false ->
                    Props;
                IC2 ->
                    [RN,{initial_call, IC2},CF]
            end
    end.
