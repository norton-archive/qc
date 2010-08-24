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
%%% File    : gmt_event_h.erl
%%% Purpose : GMT event handler
%%%----------------------------------------------------------------------

%% @doc The GMT event handler module.
%%
%% This module is responsible for taking care of general event
%% notifications, including info &amp; error logging.
%%
%% The attempts to convert SASL and other system messages to PSS-style
%% module names, severity level, etc. are not perfect.  They may need
%% to be better, but they probably do not need to be perfect.

-module(gmt_event_h).

-behaviour(gen_event).

-include("applog.hrl").

-define(HANDLER_NAME, ?MODULE).

%% External exports
-export([add_report_handler/1, delete_report_handler/0, start_singleton_report_handler/3,
         make_errt/4, make_errt/5,
         app_log/1, app_log/2, app_log/4, app_log/5,
         severity2syslog/1,
         unformat_twiddle_p/1]).
-export([subscribe/1, unsubscribe/1, set_loglevel/1, set_console_output/1,
         add_terminate_fun/1, add_exception_fun/1]).
-export([shrink/1, shrink/2]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).


%% TODO: Incomplete!  100% of public API is not yet covered.

-type property()   :: atom() | tuple().
-spec(start_singleton_report_handler/3 :: (term(), term(), list(property())) ->
             'noop' | 'ok' ).


-record(state, {
          type,                                 % pss|lss
          configfile,                           % Path to config file
          os_pid,                               % os:getpid()
          subs = [],                            % subscription list
          pss_app_log,                          % Backward compatibility
          lss_app_log,                          % Backward compatibility
          gen_app_log,                          % General app log
          custom_token,                         % Custom string
          log_fmt,                              % atom()
          date_fmt,                             % atom()
          field_sep,                            % string()
          loglevel,                             % int()
          write_to_console = true,              % bool()
          terminate_funs = [],
          exception_funs = [],
          fh
         }).

-record(errt, {
          module,
          level = ?PSS_LOG_ALERT,
          code7,
          report,                               % Format if args =:= undefined
          args                                  %io_lib format args, undefined
                                                %if report is error_logger-
                                                %style report!
         }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @spec (pss | lss | atom() | proplist()) -> term()
%% @doc Add a gmt_event_h-style event handler to the kernel's error_logger
%%      gen_event event handler.
%%
%% This function is usually called by an application callback or a
%% supervisor callback.  In the rare case of a bug in this module's
%% event or call handling, a {gen_event_EXIT, X, Y} message will be
%% sent to that module.  Unfortunately, application &amp; supervisor
%% processes will simply discard the message.  Therefore, if an
%% application wishes to do something to react to a bug in this
%% module, it should use the add_terminate_fun/1 function or
%% add_exception_fun/1 <b>after</b> calling add_report_handler/1.
%%
%% For backward compatibility, the argument may be a single atom, which
%% will control only which "central.conf" config item name is used for
%% the application log file:
%% <ul>
%% <li> pss -&lt; pss_app_log_path  </li>
%% <li> lss -&lt; lss_app_log_path  </li>
%% <li> any other atom -&lt; application_app_log_path  </li>
%% <li> any other atom -&lt; application_app_log_log_fmt  </li>
%% <li> any other atom -&lt; application_app_log_date_fmt  </li>
%% <li> any other atom -&lt; application_app_log_field_sep  </li>
%% <li> any other atom -&lt; application_app_log_custom_token  </li>
%% </ul>
%%
%% If a proplist-style properties list is specified, the following
%% properties are honored:
%% <ul>
%% <li> {log_type, X} where X may the atom pss, lss, or other (see
%%      backward compatibility above). </li>
%% <li> {log_fmt, X} where X may be the atom default | cstm1.  </li>
%%  <ul>
%%  <li> default: OS pid, date, module, severity, code7, event string</li>
%%  <li> cstm1: date, OS pid, module, severity, code7, event string</li>
%%  </ul>
%% <li> {date_fmt, X} where X may be the atom default | cstm1.  </li>
%%  <ul>
%%  <li> default: YYYYMMDDHHMMSS</li>
%%  <li> cstm1: YYYY/MM/DD HH:MM:SS:000</li> (000 is a fixed constant)
%%  </ul>
%% <li> {field_sep, N} where N is the ASCII character used to separate
%%      fields of an application log entry.  </li>
%% </ul>

add_report_handler(Type) when is_atom(Type) ->
    gen_event:add_sup_handler(error_logger, ?HANDLER_NAME, [{log_type, Type}]);
add_report_handler(PropList) when is_list(PropList) ->
    gen_event:add_sup_handler(error_logger, ?HANDLER_NAME, PropList).

delete_report_handler() ->
    error_logger:delete_report_handler(?HANDLER_NAME).

start_singleton_report_handler(Module, Type, PropList) ->
    case lists:member(gmt_event_h, gen_event:which_handlers(error_logger)) of
        true ->
            ok;
        false ->
            add_report_handler([{log_type, Type}|PropList])
    end,

    %% Disable the kernel default logger: we can't afford to have
    %% multi-gigabyte messages formatted & sent to the console.
    %% It's major effect is to remove the error_logger_tty_h handler.
    error_logger:tty(false),

    %% Disable the SASL default logger: this handler will log & print
    %% the same event but without the minor tweaks that
    %% sasl_report_tty_h does.
    gen_event:delete_handler(error_logger, sasl_report_tty_h,
                             {stop_please, ?MODULE}),
    %% Same for the default error logger.
    gen_event:delete_handler(error_logger, error_logger,
                             {stop_please, ?MODULE}),

    %% Create our timer, but only one
    NN = gmt_event_h_towner,
    case whereis(NN) of
        undefined ->
            spawn(fun() ->
                          register(NN, self()),  % May crash, that's OK.
                          timer:send_interval(1000, error_logger, reset_fh),
                          receive wait_forever -> ok end
                  end);
        _ ->
            ok
    end,
    ?APPLOG_INFO_MODULE(Module,?APPLOG_INFO_001,"start: ~p ~p",[Type, PropList]).

%% Can
make_errt(Module, Level, Code7, Report) ->
    #errt{module = Module, level = Level, code7 = Code7, report = Report}.

make_errt(Module, Level, Code7, Format, Args) ->
    #errt{module = Module, level = Level, code7 = Code7, report = Format, args = Args}.

%% @doc app_log/1 and app_log2/ are the <b>preferred</b> method to log application messages.
%% @equiv app_log(Tuple, [])

app_log({Module, Level, Code7, Format}) ->
    app_log(Module, Level, Code7, Format, []).

%% @doc app_log/1 and app_log2/ are the <b>preferred</b> method to log application messages.

app_log({Module, Level, Code7, Format}, Args) ->
    app_log(Module, Level, Code7, Format, Args).

%% @doc app_log/4 and app_log/5 may be removed, do not use them externally
%% unless absolutely necessary.

app_log(Module, Level, Code7, Report) ->
    case precheck_loglevel(Level) of
        true ->
            Errt = make_errt(Module, Level, Code7, Report),
            error_logger:error_report(gmt_event_errt, Errt);
        false ->
            noop
    end.

app_log(Module, Level, Code7, Format, Args) ->
    case precheck_loglevel(Level) of
        true ->
            Errt = make_errt(Module, Level, Code7, Format, Args),
            error_logger:error_report(gmt_event_errt, Errt);
        false ->
            noop
    end.

%% @spec (pid()) -> ok | already_subscribed
%% @doc Subscribe to copies of each event handled and formatted by
%% this event handler.
%%
%% Each subscriber will receive messages in the format:
%% <tt>{?HANDLER_NAME, #gmt_event_h_copy{}}</tt>

subscribe(Pid) ->
    gen_event:call(error_logger, ?HANDLER_NAME, {subscribe, Pid}).

%% @spec (pid()) -> ok | not_subscribed
%% @doc Cancel subscription previously created by subscribe/1.

unsubscribe(Pid) ->
    gen_event:call(error_logger, ?HANDLER_NAME, {unsubscribe, Pid}).

%% @spec (pid()) -> ok | not_subscribed
%% @doc Cancel subscription previously created by subscribe/1.

set_loglevel(LogLevel) ->
    gen_event:call(error_logger, ?HANDLER_NAME, {set_loglevel, LogLevel}).

%% @spec (boolean()) -> ok
%% @doc Specify whether log events should be sent to the console.

set_console_output(Bool) when Bool == true; Bool == false ->
    gen_event:call(error_logger, ?HANDLER_NAME, {set_console_output, Bool}).

%% @spec (fun()) -> ok
%% @doc Add a function of arity 1 (arg = reason for termination) to the
%%      list called if/when the terminate/2 callback method is called.

add_terminate_fun(Fun) when is_function(Fun, 1) ->
    gen_event:call(error_logger, ?HANDLER_NAME, {add_terminate_fun, Fun}).

%% @spec (fun()) -> ok
%% @doc Add a function of arity 1 (arg = reason for termination) to the
%%      list called if/when the terminate/2 callback method is called for an
%%      exceptional reason (not normal removal, shutdown, or module swap).

add_exception_fun(Fun) when is_function(Fun, 1) ->
    gen_event:call(error_logger, ?HANDLER_NAME, {add_exception_fun, Fun}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------
init(Props) ->
    PSSAppLog = gmt_config:get_config_value(pss_app_log_path, "/dev/null"),
    LSSAppLog = gmt_config:get_config_value(lss_app_log_path, "/dev/null"),
    LogLevel = gmt_config:get_config_value(application_app_log_level, "INFO"),
    AppLog = gmt_config:get_config_value(application_app_log_path, "/dev/null"),
    LogFmt = gmt_config:get_config_value(application_app_log_log_fmt, "default"),
    TequilaString = gmt_config:get_config_value(application_app_log_custom_token, ""),
    DateFmt = gmt_config:get_config_value(application_app_log_date_fmt, "default"),
    FieldSep = gmt_config:get_config_value_i(application_app_log_field_sep, 124),
    {ok, #state{
       type = proplists:get_value(log_type, Props, default),
       os_pid = os:getpid(),
       pss_app_log = PSSAppLog,
       lss_app_log = LSSAppLog,
       loglevel = severity2syslog(LogLevel),
       gen_app_log = AppLog,
       log_fmt = proplists:get_value(log_fmt, Props, list_to_atom(LogFmt)),
       custom_token = proplists:get_value(custom_token, Props, TequilaString),
       date_fmt = proplists:get_value(date_fmt, Props, list_to_atom(DateFmt)),
       field_sep = [proplists:get_value(field_sep, Props, FieldSep)]
      }}.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%%----------------------------------------------------------------------
handle_event(Event, State) ->
    Severity = severity2syslog(get_severity(Event)),
    if
        State#state.loglevel >= Severity ->
            try
                NewState = do_generic_event(Event, State),
                {ok, NewState}
            catch
                X:Y ->
                    Msg = io_lib:format(
                            "~s: ~p ~p: critical error: event ~P, ~P ~P from ~P\n~p\n",
                            [?MODULE, date(), time(),
                             Event, 20,
                             X, 20, Y, 20, Event, 25, erlang:get_stacktrace()]),
                    catch write_to_app_log(State, Msg),
                    catch io:format(Msg),
                    {ok, close_fh(State)}
            end;
        true ->
            {ok, State}
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%%----------------------------------------------------------------------
handle_call({subscribe, Pid}, S) ->
    case lists:member(Pid, S#state.subs) of
        true ->
            {ok, already_subscribed, S};
        false ->
            Subs = S#state.subs,
            {ok, ok, S#state{subs = [Pid|Subs]}}
    end;
handle_call({unsubscribe, Pid}, S) ->
    case lists:member(Pid, S#state.subs) of
        true ->
            Subs = S#state.subs,
            {ok, ok, S#state{subs = lists:delete(Pid, Subs)}};
        false ->
            {ok, not_subscribed, S}
    end;
handle_call({set_loglevel, LogLevel}, S) ->
    case catch severity2syslog(LogLevel) of
        Int when is_integer(Int) ->
            {ok, ok, S#state{loglevel = Int}};
        _ ->
            {ok, unkown_loglevel, S}
    end;
handle_call({set_console_output, Bool}, S) ->
    {ok, ok, S#state{write_to_console = Bool}};
handle_call({add_terminate_fun, Fun}, #state{terminate_funs = TFuns} = S) ->
    {ok, ok, S#state{terminate_funs = [Fun|TFuns]}};
handle_call({add_exception_fun, Fun}, #state{exception_funs = EFuns} = S) ->
    {ok, ok, S#state{exception_funs = [Fun|EFuns]}};
handle_call(Request, State) ->
    io:format("DEBUG: handle_call: ~p\n", [Request]),
    Reply = ok,
    {ok, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%%----------------------------------------------------------------------
handle_info(reset_fh, State) ->
    {ok, do_reset_fh(State)};
handle_info(Info, State) ->
    catch io:format("DEBUG: handle_info: ~p\n", [Info]),
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    io:format("~s:terminate: ~p ~p\n", [?MODULE, Reason, State]),
    [catch Pid ! {?HANDLER_NAME, terminate, Reason} || Pid <- State#state.subs],
    case Reason of normal          -> ok;
                   shutdown        -> ok;
                   {swapped, _, _} -> ok;
                   _               -> [spawn(fun() -> Fun(Reason) end) ||
                                          Fun <- State#state.exception_funs]
    end,
    [spawn(fun() -> Fun(Reason) end) || Fun <- State#state.terminate_funs].

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

do_generic_event(Event, State) ->
    EventPid = get_pid(Event),
    %% Ensure Module Name is ~s printable.
    Module1 = get_module(Event),
    ModuleNoPad = if
                      is_binary(Module1) ->
                          Module1;
                      is_atom(Module1) ->
                          Module1;
                      true ->
                          case lists:takewhile(fun(X)-> X >= 0 andalso X =< 255 end, Module1) of
                              Module1 ->
                                  Module1;  %% List is a string, so return as-is.
                              _ ->
                                  io_lib:format("~w", [Module1])
                          end
                  end,
    Module = gmt_util:right_pad(ModuleNoPad, 13, 32),
    Severity = get_severity(Event),
    Code7 = get_code7(Event),
    Now = now(),
    Date = calendar:now_to_local_time(Now),
    {{Yr, Mo, Da}, {Hr, Mn, Sc}} = Date,
    DateStr = if State#state.date_fmt =:= cstm1 ->
                      {_Ms, _S, Ns} = Now,
                      lists:flatten(
                        io_lib:format(
                          "~4.4.0w/~2.2.0w/~2.2.0w "
                          "~2.2.0w:~2.2.0w:~2.2.0w:~3.3.0w",
                          [Yr, Mo, Da, Hr, Mn, Sc, Ns div 1000]));
                 true ->
                      gmt_util:cal_to_bignumstr(Date)
              end,

    Sep = State#state.field_sep,
    Formatted = format_event(shrink(Event)),
    EventStr = unformat_twiddle_p(Formatted),
    S = if State#state.log_fmt =:= cstm1 ->
                io_lib:format("~s~s~s~w~s~s~s~s~s~s~p~s~s~s~s\n",
                              [DateStr, Sep,
                               State#state.custom_token, Code7, Sep,
                               State#state.custom_token, Severity, Sep,
                               State#state.os_pid, Sep,
                               EventPid, Sep,
                               Module, Sep,
                               EventStr]);
           true ->
                io_lib:format("~s~s~p~s~s~s~s~s~s~s~w~s~s\n",
                              [State#state.os_pid, Sep,
                               EventPid, Sep,
                               DateStr, Sep,
                               Module, Sep,
                               Severity, Sep,
                               Code7, Sep,
                               EventStr])
        end,

    %% only output events that don't make it to the app log
    State2 = case (catch write_to_app_log(State, S)) of
                 #state{} = St ->
                     St;
                 X ->
                     io:format("X ~p\n", [X]),
		     if Code7 =:= ?CODE7_DEFAULT ->
			     noop;
			true                    ->
			     io:put_chars("-->"),
			     io:format("~P", [X, 30]),
			     io:put_chars("<--\n")
		     end,
		     State
             end,

    %% Do what the error_logger_tty_h handler does, sortof.  Make it
    %% look different, so we can tell our messages apart from
    %% error_logger_tty_h's messages.
    if State2#state.write_to_console ->
            io:format("~n=GMT ~s REPORT==== ~p-~s-~p::~s:~s:~s ===~n",
                      [Severity, Da, month(Mo), Yr, t(Hr), t(Mn), t(Sc)]),
            io:put_chars(Formatted);
       true ->
            ok
    end,

    if State2#state.subs =/= [] ->
            Copy = #gmt_event_h_copy{
              now = Now,
              date = Date,
              date_str = DateStr,
              os_pid = State2#state.os_pid,
              module = ModuleNoPad,
              severity = Severity,
              syslog_facility = severity2syslog(Severity),
              code7 = Code7,
              event_str = EventStr,
              raw_event = Event
             },
            [catch Pid ! {?HANDLER_NAME, Copy} || Pid <- State2#state.subs];
       true ->
            ok
    end,

    State2.

format_event({_T, _GL, {_Pid, gmt_event_errt, E}}) when is_record(E, errt) ->
    if E#errt.args =:= undefined ->
            %% Style = Report term
            lists:flatten(io_lib:format("~P", [E#errt.report,
                                               ?FORMAT_MAXDEPTH]));
       true ->
            %% Style =:= Format + Args
            lists:flatten(io_lib:format(E#errt.report, E#errt.args))
    end;
format_event({T, _GL, {_Pid, Format, Data}})
  when T =:= error; T =:= warning_msg; T =:= info_msg ->
    Size = byte_size(term_to_binary(Data)),  % Drat, I hate to do this, but...
    if Size =< 10*1000 ->
            case (catch lists:flatten(io_lib:format(Format, Data))) of
                {'EXIT', _} ->
                    lists:flatten(io_lib:format("BAD FORMATTING: Format = '~s', Data = ~p\n", [Format, Data]));
                Str ->
                    Str
            end;
       true ->
            M = io_lib:format("Large event message: Size = ~p, T = ~p, "
                              "Format = '~s', Data = ~P",
                              [Size, T, lists:flatten(Format), Data, ?FORMAT_MAXDEPTH]),
            lists:flatten(M)
    end;
format_event({_T, _GL, {_Pid, Type, ReportTerm}}) ->
    lists:flatten(io_lib:format("~P: ~P", [Type, ?FORMAT_MAXDEPTH,
                                           ReportTerm, ?FORMAT_MAXDEPTH]));
format_event(SomeOtherTerm) ->
    lists:flatten(io_lib:format("GMT unknown event: ~P", [SomeOtherTerm, ?FORMAT_MAXDEPTH])).

get_pid({_T, _GL, {Pid, gmt_event_errt, _Report}}) ->
    Pid;
get_pid({_T, _GL, {Pid, _FormatOrType, _DataOrReport}}) ->
    Pid;
get_pid(_) ->
    0.

get_module({_T, _GL, {_Pid, gmt_event_errt, Report}}) ->
    errt_module(Report);
get_module({error, _GL, {_Pid, Format, _Data}}) ->
    case string:str(string:substr(Format, 1, 10), "Mnesia") of
        0 -> "DEFAULT";
        _ -> "MNESIA"
    end;
get_module({_RepType, _GL, {_Pid, progress, _Report}}) ->
    "SASL";
get_module(_) ->
    "DEFAULT".

get_severity({_T, _GL, {_Pid, gmt_event_errt, Report}}) ->
    errt_level(Report);
get_severity({warning_msg, _GL, _Blah}) ->
    ?LOG_WARNING;
get_severity({warning_report, _GL, _Blah}) ->
    ?LOG_WARNING;
get_severity({error, _GL, _Blah}) ->
    ?LOG_ERR;
get_severity({error_report, _GL, {_Pid, supervisor_report, _Report}}) ->
    ?LOG_ERR;
get_severity({error_report, _GL, {_Pid, crash_report, _Report}}) ->
    ?LOG_ERR;
get_severity({error_report, _GL, _Blah}) ->
    ?LOG_ERR;
get_severity({info_msg, _GL, _Blah}) ->
    ?LOG_INFO;
get_severity({info_report, _GL, _Blah}) ->
    ?LOG_INFO;
get_severity(_) ->
    ?LOG_INFO.

severity2syslog(?LOG_EMERG) ->   0;
severity2syslog(?LOG_ALERT) ->   1;
severity2syslog(?LOG_CRIT) ->    2;
severity2syslog(?LOG_ERR) ->     3;
severity2syslog(?LOG_WARNING) -> 4;
severity2syslog(?LOG_NOTICE) ->  5;
severity2syslog(?LOG_INFO) ->    6;
severity2syslog(?LOG_DEBUG) ->   7.

get_code7({_T, _GL, {_Pid, gmt_event_errt, Report}}) ->
    errt_code7(Report);
get_code7(_) ->
    ?CODE7_DEFAULT.

errt_module(E) ->
    E#errt.module.

errt_level(E) ->
    E#errt.level.

errt_code7(E) ->
    E#errt.code7.

%% The caller should call inside a "catch" if worried about crashing.

write_to_app_log(#state{type = Type} = S, Str)
  when Type == pss; Type == lss ->         %% GMT backward compatibility clause
    Path = if S#state.type =:= pss -> S#state.pss_app_log;
              S#state.type =:= lss -> S#state.lss_app_log
           end,
    {ok, F} = file:open(Path, [append, raw]),
    file:write(F, Str),
    file:close(F),
    S;
write_to_app_log(#state{fh = undefined} = S, Str) ->
    {ok, FH} = file:open(S#state.gen_app_log,
                         [append, raw, {delayed_write, 128*1024, 500}]),
    write_to_app_log(S#state{fh = FH}, Str);
write_to_app_log(S, Str) ->
    ok = file:write(S#state.fh, Str),
    S.

precheck_loglevel(Severity) ->
    LogLevel = case catch gmt_config_svr:get_config_value(application_app_log_level, "INFO") of
                   {ok, LL} ->
                       LL;
                   _Err ->
                       gmt_config:get_config_value(application_app_log_level, "INFO")
               end,
    IntLogLevel = severity2syslog(LogLevel),
    IntSeverity = severity2syslog(Severity),
    IntLogLevel >= IntSeverity.

unformat_twiddle_p(S) ->
    Lines = string:tokens(S, "\r\n"),
    Lines1 = [ string:strip(L) || L <- Lines ],
    Lines2 = lists:flatten(Lines1),
    string:strip(Lines2, both, $").

t(X) when is_integer(X) ->
    t1(integer_to_list(X)).

t1([X]) -> [$0,X];
t1(X)   -> X.

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%% Shrink the size of printable strings.
%% !@#$! formatting for "~P" will *not* limit their size.

shrink(T) ->
    shrink(T, 256).

shrink(L, MaxLen) when is_list(L) ->
    case io_lib:char_list(L) of
        true ->
            if length(L) > MaxLen ->
                    string:substr(L, 1, MaxLen) ++ "...truncated.";
               true ->
                    L
            end;
        false ->
            improper_map(fun(X) -> shrink(X, MaxLen) end, L)
    end;
shrink(T, MaxLen) when is_tuple(T) ->
    list_to_tuple(shrink(tuple_to_list(T), MaxLen));
shrink(B, MaxLen) when is_binary(B) ->
    if byte_size(B) > MaxLen ->
            <<Prefix:MaxLen/binary, _/binary>> = B,
            list_to_binary([Prefix, "...truncated."]);
       true ->
            B
    end;
shrink(T, _MaxLen) ->
    T.

improper_map(F, L) ->
    improper_map(F, L, []).

improper_map(_F, [], Acc) ->
    lists:reverse(Acc);
improper_map(F, [H|T], Acc) when is_list(T) ->
    improper_map(F, T, [F(H)|Acc]);
improper_map(F, [H|T], Acc) when not is_list(T) ->
    lists:reverse(Acc) ++ [F(H)|F(T)].

do_reset_fh(#state{fh = undefined} = S) ->
    S;
do_reset_fh(S) ->
    close_fh(S).

close_fh(S) ->
    catch file:close(S#state.fh),
    S#state{fh = undefined}.
