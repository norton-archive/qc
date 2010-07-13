%%%-------------------------------------------------------------------
%%% Copyright (c) 2007-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_applog.hrl
%%% Purpose : GMT application log
%%%----------------------------------------------------------------------

-ifndef(gmt_applog).
-define(gmt_applog, true).

%%
%% Please see here for Gemini's Error Code policies
%%
%% https://inet.geminimobile.com/twiki/bin/view/Main/ErrorMessageDocs
%%


%%
%% @doc Gemini Log Level Strings
%%

-define(LOG_EMERG,   "EMERG").
-define(LOG_ALERT,   "ALERT").
-define(LOG_CRIT,    "CRIT").
-define(LOG_ERR,     "ERR").
-define(LOG_WARNING, "WARNG").
-define(LOG_NOTICE,  "NOTIC").
-define(LOG_INFO,    "INFO").
-define(LOG_DEBUG,   "DEBUG").

%% We use the same numbering as syslog(3), easier to remember in the field...
-define(LOG_EMERG_PRI,   0).
-define(LOG_ALERT_PRI,   1).
-define(LOG_CRIT_PRI,    2).
-define(LOG_ERR_PRI,     3).
-define(LOG_WARNING_PRI, 4).
-define(LOG_NOTICE_PRI,  5).
-define(LOG_INFO_PRI,    6).
-define(LOG_DEBUG_PRI,   7).

%%
%% @doc Erlang Error Code Product/Applications
%%

%% @doc 21x00zz         GCX PSS/LSS
-define(APPLOG_000, 2100000).

%% @doc 21x01zz         Erlang app: src/erl-custom/cmcc/mmsg-msgid-db
-define(APPLOG_001, 2100100).

%% @doc 21x02zz         Erlang app: src/erl-custom/cmcc/mmsg-msgid-gen
-define(APPLOG_002, 2100200).

%% @doc 21x03zz         Erlang app: src/erl-apps/gmt-util
-define(APPLOG_003, 2100300).

%% @doc 21x04zz         Erlang app: src/erl-apps/mnesia-diskmon
-define(APPLOG_004, 2100400).

%% @doc 21x05zz         Erlang app: src/erl-apps/partition-detector
-define(APPLOG_005, 2100500).

%% @doc 21x06zz         Erlang app: src/erl-apps/ticket-counter
-define(APPLOG_006, 2100600).

%% @doc 21x07zz         Erlang app: src/erl-apps/lss-exp
-define(APPLOG_007, 2100700).

%% @doc 21x08zz         Erlang app: src/erl-apps/wap-ppg
-define(APPLOG_008, 2100800).

%%
%% NOTE: wap-ppg uses more than 100 code
%% DON'T USE APPLOG_009
%%

%% @doc 21x10zz         Erlang app: src/erl-custom/docomo/mgs-userdb
-define(APPLOG_010, 2101000).

%% @doc 21x11zz         Erlang app: src/erl-apps/raddb
-define(APPLOG_011, 2101100).

%% @doc 21x12zz         Erlang app: src/erl-third-party/trapexit/iserve
-define(APPLOG_012, 2101200).

%% @doc 21x13zz         Erlang app: src/erl-apps/congestion-watcher
-define(APPLOG_013, 2101300).

%% @doc 21x14zz         Erlang app: src/erl-custom/docomo/mgs-db
-define(APPLOG_014, 2101400).

%% @doc 21x15zz         Erlang app: src/erl-apps/delivery-queue
-define(APPLOG_015, 2101500).

%%
%% NOTE: delivery-queue uses more than 100 code
%% DON'T USE APPLOG_016
%%

%% @doc 21x17zz         Erlang app: src/erl-apps/oserl-smsc
-define(APPLOG_017, 2101700).

%% @doc 21x18zz         Erlang app: src/erl-custom/docomo/dque-db
-define(APPLOG_018, 2101800).

%% @doc 21x19zz         Erlang app: src/erl-demos/hcs-proxy
-define(APPLOG_019, 2101900).

%% @doc 21x20zz         Erlang app: src/erl-apps/term-queue
-define(APPLOG_020, 2102000).

%%
%% NOTE: term-queue uses more than 100 code
%% DON'T USE APPLOG_021
%%

%% @doc 21x22zz         Erlang app: src/erl-apps/term-delivery
-define(APPLOG_022, 2102200).

%% @doc 21x23zz         Erlang app: src/erl-apps/a2s
-define(APPLOG_023, 2102300).

%% @doc 21x24zz         Erlang app: src/erl-apps/m2be
-define(APPLOG_024, 2102400).

%% @doc 21x25zz         Erlang app: src/erl-apps/m2fe
-define(APPLOG_025, 2102500).

%% @doc 21x26zz         Erlang app: src/erl-apps/m2si
-define(APPLOG_026, 2102600).

%% @doc 21x27zz         Erlang app: src/erl-apps/gdss
-define(APPLOG_027, 2102700).

%% @doc 21x28zz         Erlang app: src/erl-custom/nttr/m2h
-define(APPLOG_028, 2102800).


%% @doc 219zzz          GCX PSS/LSS (misc debug level)
-define(APPLOG_900, 2190000).

%% @doc 2199999         Erlang app: Unclassifiable message (usually originating from 3rd party code)
-define(APPLOG_999, 2199999).


%%
%% @doc Erlang Error Code Classes
%%

%%   210yyzz            SSSR - Startup, Shutdown, Signal, Restart indication
-define(APPLOG_CLASS_SSSR,     0).

%%   211yyzz            CONF - Configuration error
-define(APPLOG_CLASS_CONF, 10000).

%%   212yyzz            SYSR - System resource error
-define(APPLOG_CLASS_SYSR, 20000).

%%   213yyzz            FILE - File resource error
-define(APPLOG_CLASS_FILE, 30000).

%%   214yyzz            NETW - Network resource error
-define(APPLOG_CLASS_NETW, 40000).

%%   215yyzz            OTPR - OTP resource error
-define(APPLOG_CLASS_OTPR, 50000).

%%   216yyzz            TBD1 - Not used
-define(APPLOG_CLASS_TBD1, 60000).

%%   217yyzz            TBD2 - Not used
-define(APPLOG_CLASS_TBD2, 70000).

%%   218yyzz            APPM - Application Message
-define(APPLOG_CLASS_APPM, 80000).

%%   219yyzz            INFO - Informational Message
-define(APPLOG_CLASS_INFO, 90000).

%%
%% @doc Erlang APPLOG Event Macros
%%

 -define(gmt_applog_info, true).


%%
%% DO NOT ENABLE HERE - please enable via erlc command line arguments
%%
%%   (-Dgmt_applog_debug)
%%
%%-define(gmt_applog_debug, true).


%% DO NOT ENABLE HERE - please enable via erlc command line arguments
%%
%%   (-Dgmt_applog_verbose_debug)
%%
%%-define(gmt_applog_verbose_debug, true).


%%
%% arguments of APPLOG_* macros:
%%
%% APPLOG_{ALERT,WARNING,INFO}        (Code,Format,Args)
%% APPLOG_{ALERT,WARNING,INFO}_NOARGS (Code,Format)
%%
%% APPLOG_{ALERT,WARNING,INFO}_MODULE        (Module,Code,Format,Args)
%% APPLOG_{ALERT,WARNING,INFO}_MODULE_NOARGS (Module,Code,Format)
%%
%%
%% APPLOG_{DEBUG,GMT_DEBUG}        (Format,Args)
%% APPLOG_{DEBUG,GMT_DEBUG}_NOARGS (Format)
%%
%% APPLOG_{DEBUG,GMT_DEBUG}_MODULE        (Module,Format,Args)
%% APPLOG_{DEBUG,GMT_DEBUG}_MODULE_NOARGS (Module,Format)
%%
%%
%% APPLOG_{ALERT,WARNING,NOTICE,INFO}_TODO (Code,Format,Args)
%% APPLOG_{ALERT,WARNING,NOTICE,INFO}_NOARGS_TODO (Code,Format)
%%
%% APPLOG_{ALERT,WARNING,NOTICE,INFO}_MODULE_TODO (Module,Code,Format,Args)
%% APPLOG_{ALERT,WARNING,NOTICE,INFO}_MODULE_NOARGS_TODO (Module,Code,Format)
%%

%% helper macro: GMT_APPLOG_CODE_
-ifdef(gmt_applog_extract_erl_errorcodes).
    %% this macro is tricky. contains extra ','. it is intensional.
    -define(GMT_APPLOG_CODE_(Code), ??Code,).
-else.
    %% this macro is tricky. definition part is empty. it is intensional.
    -define(GMT_APPLOG_CODE_(Code),).
-endif.

%% helper macro: GMT_APPLOG_ and GMT_APPLOG_NA_
-define(GMT_APPLOG_(Module,Level,Code,Format,Args),
    ?GMT_APPLOG_CODE_(Code) gmt_event_h:app_log({Module,Level,Code,Format},Args)).
-define(GMT_APPLOG_NA_(Module,Level,Code,Format),
    ?GMT_APPLOG_CODE_(Code) gmt_event_h:app_log({Module,Level,Code,Format})).

%%
%% APPLOG_{ALERT,WARNING,NOTICE,INFO}{,_NOARGS,_MODULE,_MODULE_NOARGS}
%%
-define(APPLOG_ALERT(Code,Format,Args),
        ?GMT_APPLOG_(?MODULE_STRING,?LOG_ALERT,  Code,Format,Args)).

-define(APPLOG_WARNING(Code,Format,Args),
        ?GMT_APPLOG_(?MODULE_STRING,?LOG_WARNING,Code,Format,Args)).

-define(APPLOG_NOTICE(Code,Format,Args),
        ?GMT_APPLOG_(?MODULE_STRING,?LOG_NOTICE ,Code,Format,Args)).


%% *_NOARGS
-define(APPLOG_ALERT_NOARGS(Code,Format),
        ?GMT_APPLOG_NA_(?MODULE_STRING,?LOG_ALERT,  Code,Format)).

-define(APPLOG_WARNING_NOARGS(Code,Format),
        ?GMT_APPLOG_NA_(?MODULE_STRING,?LOG_WARNING,Code,Format)).

-define(APPLOG_NOTICE_NOARGS(Code,Format),
        ?GMT_APPLOG_NA_(?MODULE_STRING,?LOG_NOTICE, Code,Format)).


%% *_MODULE
-define(APPLOG_ALERT_MODULE(Module,Code,Format,Args),
        ?GMT_APPLOG_(gmt_util:list_ify(Module),?LOG_ALERT,  Code,Format,Args)).

-define(APPLOG_WARNING_MODULE(Module,Code,Format,Args),
        ?GMT_APPLOG_(gmt_util:list_ify(Module),?LOG_WARNING,Code,Format,Args)).

-define(APPLOG_NOTICE_MODULE(Module,Code,Format,Args),
        ?GMT_APPLOG_(gmt_util:list_ify(Module),?LOG_NOTICE, Code,Format,Args)).


%% *_MODULE_NOARGS
-define(APPLOG_ALERT_MODULE_NOARGS(Module,Code,Format),
        ?GMT_APPLOG_NA_(gmt_util:list_ify(Module),?LOG_ALERT,  Code,Format)).

-define(APPLOG_WARNING_MODULE_NOARGS(Module,Code,Format),
        ?GMT_APPLOG_NA_(gmt_util:list_ify(Module),?LOG_WARNING,Code,Format)).

-define(APPLOG_NOTICE_MODULE_NOARGS(Module,Code,Format),
        ?GMT_APPLOG_NA_(gmt_util:list_ify(Module),?LOG_NOTICE, Code,Format)).



%%
%% APPLOG_{DEBUG,GMT_DEBUG}{,_NOARGS,_MODULE,_MODULE_NOARGS}
%%

%% helper macro: GMT_APPLOG_DBG_, GMT_APPLOG_DBG_NA_


-ifdef(gmt_applog_debug).
    -define(GMT_APPLOG_DBG_(Module,Format,Args),
        gmt_event_h:app_log({Module,?LOG_DEBUG,-1*?LINE,Format},Args)).
    -define(GMT_APPLOG_DBG_NA_(Module,Format),
        gmt_event_h:app_log({Module,?LOG_DEBUG,-1*?LINE,Format})).
-else.
    -define(GMT_APPLOG_DBG_(Module,Format,Args),noop).
    -define(GMT_APPLOG_DBG_NA_(Module,Format),noop).
-endif.


-ifndef(gmt_applog_extract_erl_errorcodes).
        -ifdef(gmt_applog_info).
                -define(APPLOG_INFO_MODULE(Module,Code,Format,Args), ?GMT_APPLOG_(gmt_util:list_ify(Module),?LOG_INFO,   Code,Format,Args)).
                -define(APPLOG_INFO_NOARGS(Code,Format), ?GMT_APPLOG_NA_(?MODULE_STRING,?LOG_INFO,   Code,Format)).
                -define(APPLOG_INFO_MODULE_NOARGS(Module,Code,Format), ?GMT_APPLOG_NA_(gmt_util:list_ify(Module),?LOG_INFO,   Code,Format)).
                -define(APPLOG_INFO(Code,Format,Args), ?GMT_APPLOG_(?MODULE_STRING,?LOG_INFO,   Code,Format,Args)).
        -else.
                -define(APPLOG_INFO_MODULE(Module,Code,Format,Args), noop).
                -define(APPLOG_INFO_NOARGS(Code,Format), noop).
                -define(APPLOG_INFO_MODULE_NOARGS(Module,Code,Format), noop).
                -define(APPLOG_INFO(Code,Format,Args), noop).
        -endif.
-else.
        -define(APPLOG_INFO_MODULE(Module,Code,Format,Args), ?GMT_APPLOG_(gmt_util:list_ify(Module),?LOG_INFO,   Code,Format,Args)).
        -define(APPLOG_INFO_NOARGS(Code,Format), ?GMT_APPLOG_NA_(?MODULE_STRING,?LOG_INFO,   Code,Format)).
        -define(APPLOG_INFO_MODULE_NOARGS(Module,Code,Format), ?GMT_APPLOG_NA_(gmt_util:list_ify(Module),?LOG_INFO,   Code,Format)).
        -define(APPLOG_INFO(Code,Format,Args), ?GMT_APPLOG_(?MODULE_STRING,?LOG_INFO,   Code,Format,Args)).
-endif.


%% helper macro: GMT_APPLOG_VDBG_, GMT_APPLOG_VDBG_NA_
-ifdef(gmt_applog_verbose_debug).
    -define(GMT_APPLOG_VDBG_(Module,Format,Args),
        gmt_event_h:app_log({Module,?LOG_DEBUG,-1*?LINE,Format},Args)).
    -define(GMT_APPLOG_VDBG_NA_(Module,Format),
        gmt_event_h:app_log({Module,?LOG_DEBUG,-1*?LINE,Format})).
-else.
    -define(GMT_APPLOG_VDBG_(Module,Format,Args),noop).
    -define(GMT_APPLOG_VDBG_NA_(Module,Format),noop).
-endif.


-define(APPLOG_DEBUG(Format,Args), ?GMT_APPLOG_DBG_(?MODULE_STRING,Format,Args)).
-define(APPLOG_GMT_DEBUG(Format,Args), ?GMT_APPLOG_VDBG_(?MODULE_STRING,Format,Args)).

-define(APPLOG_DEBUG_NOARGS(Format), ?GMT_APPLOG_DBG_NA_(?MODULE_STRING,Format)).
-define(APPLOG_GMT_DEBUG_NOARGS(Format), ?GMT_APPLOG_VDBG_NA_(?MODULE_STRING,Format)).

-define(APPLOG_DEBUG_MODULE(Module,Format,Args), ?GMT_APPLOG_DBG_(gmt_util:list_ify(Module),Format,Args)).
-define(APPLOG_GMT_DEBUG_MODULE(Module,Format,Args), ?GMT_APPLOG_VDBG_(gmt_util:list_ify(Module),Format,Args)).

-define(APPLOG_DEBUG_MODULE_NOARGS(Module,Format), ?GMT_APPLOG_DBG_NA_(gmt_util:list_ify(Module),Format)).
-define(APPLOG_GMT_DEBUG_MODULE_NOARGS(Module,Format), ?GMT_APPLOG_VDBG_NA_(gmt_util:list_ify(Module),Format)).


%%
%% @doc Erlang **TODO** APPLOG Event Macros
%%

%% Uncomment next line to find all pending TODO applogs
%%-define(gmt_applog_disable_todo, true).

%% APPLOG_{ALERT,WARNING,NOTICE,INFO}_TODO (Code,Format,Args)
%% APPLOG_{ALERT,WARNING,NOTICE,INFO}_NOARGS_TODO (Code,Format)
%% APPLOG_{ALERT,WARNING,NOTICE,INFO}_MODULE_TODO (Module,Code,Format,Args)
%% APPLOG_{ALERT,WARNING,NOTICE,INFO}_MODULE_NOARGS_TODO (Module,Code,Format)

-ifndef(gmt_applog_disable_todo).

-define(APPLOG_ALERT_TODO(Code,Format,Args),
        gmt_event_h:app_log({?MODULE_STRING,?LOG_ALERT,-1*?LINE,Format},Args)).
-define(APPLOG_WARNING_TODO(Code,Format,Args),
        gmt_event_h:app_log({?MODULE_STRING,?LOG_WARNING,-1*?LINE,Format},Args)).
-define(APPLOG_NOTICE_TODO(Code,Format,Args),
        gmt_event_h:app_log({?MODULE_STRING,?LOG_NOTICE,-1*?LINE,Format},Args)).
-define(APPLOG_INFO_TODO(Code,Format,Args),
        gmt_event_h:app_log({?MODULE_STRING,?LOG_INFO,-1*?LINE,Format},Args)).

-define(APPLOG_ALERT_NOARGS_TODO(Code,Format),
        gmt_event_h:app_log({?MODULE_STRING,?LOG_ALERT,-1*?LINE,Format})).
-define(APPLOG_WARNING_NOARGS_TODO(Code,Format),
        gmt_event_h:app_log({?MODULE_STRING,?LOG_WARNING,-1*?LINE,Format})).
-define(APPLOG_NOTICE_NOARGS_TODO(Code,Format),
        gmt_event_h:app_log({?MODULE_STRING,?LOG_NOTICE,-1*?LINE,Format})).
-define(APPLOG_INFO_NOARGS_TODO(Code,Format),
        gmt_event_h:app_log({?MODULE_STRING,?LOG_INFO,-1*?LINE,Format})).

-define(APPLOG_ALERT_MODULE_TODO(Module,Code,Format,Args),
        gmt_event_h:app_log({gmt_util:list_ify(Module),?LOG_ALERT,-1*?LINE,Format},Args)).
-define(APPLOG_WARNING_MODULE_TODO(Module,Code,Format,Args),
        gmt_event_h:app_log({gmt_util:list_ify(Module),?LOG_WARNING,-1*?LINE,Format},Args)).
-define(APPLOG_NOTICE_MODULE_TODO(Module,Code,Format,Args),
        gmt_event_h:app_log({gmt_util:list_ify(Module),?LOG_NOTICE,-1*?LINE,Format},Args)).
-define(APPLOG_INFO_MODULE_TODO(Module,Code,Format,Args),
        gmt_event_h:app_log({gmt_util:list_ify(Module),?LOG_INFO,-1*?LINE,Format},Args)).

-define(APPLOG_ALERT_MODULE_NOARGS_TODO(Module,Code,Format),
        gmt_event_h:app_log({gmt_util:list_ify(Module),?LOG_ALERT,-1*?LINE,Format})).
-define(APPLOG_WARNING_MODULE_NOARGS_TODO(Module,Code,Format),
        gmt_event_h:app_log({gmt_util:list_ify(Module),?LOG_WARNING,-1*?LINE,Format})).
-define(APPLOG_NOTICE_MODULE_NOARGS_TODO(Module,Code,Format),
        gmt_event_h:app_log({gmt_util:list_ify(Module),?LOG_NOTICE,-1*?LINE,Format})).
-define(APPLOG_INFO_MODULE_NOARGS_TODO(Module,Code,Format),
        gmt_event_h:app_log({gmt_util:list_ify(Module),?LOG_INFO,-1*?LINE,Format})).

-endif. % -ifndef(gmt_applog_disable_todo).

-endif. % -ifndef(gmt_applog).
