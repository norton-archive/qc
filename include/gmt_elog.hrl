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
%%% File    : gmt_elog.erl
%%% Purpose : GMT event log
%%%----------------------------------------------------------------------

%%% These macros are merely examples.  It's likely that apps may want
%%% to use their own macros rather than these ... that's OK.

-ifndef(gmt_elog_hrl).
-define(gmt_elog_hrl, true).

-include("gmt_applog.hrl").

%%
%% Events without a category
%%

-define(ELOG_EMERG(Fmt, Args),
        gmt_elog_policy:enabled(?LOG_EMERG_PRI, none, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_ALERT(Fmt, Args),
        gmt_elog_policy:enabled(?LOG_ALERT_PRI, none, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_CRIT(Fmt, Args),
        gmt_elog_policy:enabled(?LOG_CRIT_PRI, none, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_ERR(Fmt, Args),
        gmt_elog_policy:enabled(?LOG_ERR_PRI, none, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_WARNING(Fmt, Args),
        gmt_elog_policy:enabled(?LOG_WARNING_PRI, none, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_NOTICE(Fmt, Args),
        gmt_elog_policy:enabled(?LOG_NOTICE_PRI, none, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_INFO(Fmt, Args),
        gmt_elog_policy:enabled(?LOG_INFO_PRI, none, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_DEBUG(Fmt, Args),
        gmt_elog_policy:enabled(?LOG_DEBUG_PRI, none, ?MODULE, ?LINE, Fmt, Args)).

%%
%% Events with a category
%%

-define(ELOG_EMERG_C(Cat, Fmt, Args),
        gmt_elog_policy:enabled(?LOG_EMERG_PRI, Cat, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_ALERT_C(Cat, Fmt, Args),
        gmt_elog_policy:enabled(?LOG_ALERT_PRI, Cat, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_CRIT_C(Cat, Fmt, Args),
        gmt_elog_policy:enabled(?LOG_CRIT_PRI, Cat, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_ERR_C(Cat, Fmt, Args),
        gmt_elog_policy:enabled(?LOG_ERR_PRI, Cat, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_WARNING_C(Cat, Fmt, Args),
        gmt_elog_policy:enabled(?LOG_WARNING_PRI, Cat, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_NOTICE_C(Cat, Fmt, Args),
        gmt_elog_policy:enabled(?LOG_NOTICE_PRI, Cat, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_INFO_C(Cat, Fmt, Args),
        gmt_elog_policy:enabled(?LOG_INFO_PRI, Cat, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_DEBUG_C(Cat, Fmt, Args),
        gmt_elog_policy:enabled(?LOG_DEBUG_PRI, Cat, ?MODULE, ?LINE, Fmt, Args)).

%%
%% Here's an example of using the method where gmt_elog_policy:enabled/6
%% function is redefined.
%%

%% NOTE: This is a macro, beware of stray variable bindings inside "Code"!!

-define(ELOG_CODE(Pri, Cat, Code),
        case gmt_elog_policy:enabled(Pri, Cat, ?MODULE, ?LINE, x, x) of
            true  -> Code;
            false -> ok
        end).

-endif. % -ifndef(gmt_elog_hrl).
