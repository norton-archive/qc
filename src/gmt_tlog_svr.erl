%%%----------------------------------------------------------------------
%%% Copyright (c) 2009-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_tlog_svr.erl
%%% Purpose : GMT transaction log server
%%%----------------------------------------------------------------------

-module(gmt_tlog_svr).
-behaviour(gen_server).

-include("gmt_tlog_svr.hrl").

-export([start_link/0]).

-export([tlog_reopen/0]).

-export([tlog/1, tlog/2]).
-export([tlog/0, tlog_put/1, tlog_get/0, tlog_add/1]).

-export([tlog_duration/1, tlog_duration/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
          filename=undefined
          , fd=undefined
          , flush=0
          , acc=[]
          , acclen=0
         }).

-define(TLOG_CONFIG_PATH, application_tx_log_path).
-define(TLOG_CONFIG_FLUSH, application_tx_log_flush).

-define(TLOG_KEY_EVENT, {?MODULE,event}).
-define(TLOG_KEY_PRINT, {?MODULE,print}).


%% external functions -----------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

tlog_reopen() ->
    gen_server:call(?MODULE, reopen).

tlog() ->
    case tlog_get() of
        undefined ->
            ok;
        Event ->
            tlog(Event)
    end.

tlog(Event) ->
    tlog(Event, now()).

tlog(Event, Now) when is_record(Event, tlog_core) ->
    case get(?TLOG_KEY_PRINT) of
        true ->
            ok;
        _ ->
            try
                ?MODULE ! {fevent, gmt_tlog_printer:tlog_format(Event, Now)}
            catch
                error:badarg ->
                    ok;
                error:undef ->
                    put(?TLOG_KEY_PRINT, true);
                _:_ ->
                    noop %% not really ok, but nothing else to do here
            end,
            ok
    end.


%% interfaces with process dictionary
tlog_put(Event) when is_record(Event, tlog_core) ->
    put(?TLOG_KEY_EVENT, Event).

tlog_add(#tlog_core{extras=New}=Event) ->
    case get(?TLOG_KEY_EVENT) of
        undefined ->
            put(?TLOG_KEY_EVENT, Event);
        Old when is_list(Old) ->
            put(?TLOG_KEY_EVENT, Event#tlog_core{extras=New++Old})
    end;
tlog_add(New) when is_list(New) ->
    case get(?TLOG_KEY_EVENT) of
        undefined ->
            put(?TLOG_KEY_EVENT, New);
        Old when is_list(Old) ->
            put(?TLOG_KEY_EVENT, New++Old);
        #tlog_core{extras=Old}=Event ->
            put(?TLOG_KEY_EVENT, Event#tlog_core{extras=New++Old})
    end.

tlog_get() ->
    erase(?TLOG_KEY_EVENT).


tlog_duration(StartTime) ->
    tlog_duration(StartTime, now()).

tlog_duration(undefined, Now) ->
    {0, Now};
tlog_duration(StartTime, Now) ->
    Duration = timer:now_diff(Now, StartTime) div 1000,
    {Duration, Now}.


%% gen_server callbacks --------------------------------------------------

init(_) ->
    {ok, Filename} = gmt_config_svr:get_config_value(?TLOG_CONFIG_PATH, "/dev/null"),
    {ok, do_open(#state{filename=Filename})}.

handle_call(reopen, _From, State) ->
    {reply, ok, do_reopen(State)};
handle_call(Msg, _From, State) ->
    error_logger:error_msg("~s:handle_call: ~p: got call ~P\n",
                           [?MODULE, self(), Msg, 20]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    error_logger:error_msg("~s:handle_cast: ~p: got cast ~P\n",
                           [?MODULE, self(), Msg, 20]),
    {noreply, State}.

handle_info({fevent, FEvent}, #state{fd=Fd,flush=Flush,acc=Acc,acclen=AccLen}=State)
  when is_binary(FEvent); is_list(FEvent) ->
    {NewAcc, NewAccLen} = get_more_fevents([FEvent|Acc], AccLen+1),
    if NewAccLen < Flush ->
            {noreply, State#state{acc=NewAcc,acclen=NewAccLen}};
       true ->
            ok = file:write(Fd, lists:reverse(NewAcc)),
            {noreply, State#state{acc=[],acclen=0}}
    end;
handle_info(Info, State) ->
    error_logger:error_msg("~s:handle_info: ~p: got info ~P\n",
                           [?MODULE, self(), Info, 20]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    catch do_close(State).


%% internal functions -----------------------------------------------------------

do_reopen(#state{acclen=0}=State) ->
    do_open(do_close(State));
do_reopen(#state{fd=Fd,acc=Acc}=State) ->
    ok = file:write(Fd, lists:reverse(Acc)),
    do_open(do_close(State#state{acc=[],acclen=0})).

do_open(#state{filename=Filename}=State) ->
    {ok, Flush} = gmt_config_svr:get_config_value_i(?TLOG_CONFIG_FLUSH, 0),
    case file:open(Filename, [append,raw,binary,delayed_write]) of
        {ok, Fd} ->
            State#state{fd=Fd,flush=Flush};
        _ ->
            exit({file, open, Filename})
    end.

do_close(#state{fd=Fd}=State) ->
    case file:close(Fd) of
        ok ->
            noop;
        {error, enospc} ->
            ok = file:close(Fd)
    end,
    State#state{fd=undefined}.

get_more_fevents(Acc, AccLen) ->
    receive
        {fevent, FEvent} ->
            get_more_fevents([FEvent|Acc], AccLen + 1)
    after 0 ->
            {Acc, AccLen}
    end.

