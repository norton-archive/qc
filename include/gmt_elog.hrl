%%%----------------------------------------------------------------------
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
%%% File    : gmt_elog.erl
%%% Purpose : GMT event log
%%%----------------------------------------------------------------------

-ifndef(gmt_elog_hrl).
-define(gmt_elog_hrl, true).

%%
%% Events with a message
%%

-define(ELOG_ERROR(Msg),
        gmt_elog_policy:enabled(error, undefined, ?MODULE, ?LINE, Msg, [])).
-define(ELOG_WARNING(Msg),
        gmt_elog_policy:enabled(warning, undefined, ?MODULE, ?LINE, Msg, [])).
-define(ELOG_INFO(Msg),
        gmt_elog_policy:enabled(info, undefined, ?MODULE, ?LINE, Msg, [])).
-define(ELOG_DEBUG(Msg),
        gmt_elog_policy:enabled(debug, undefined, ?MODULE, ?LINE, Msg, [])).

%%
%% Events with a format and args
%%

-define(ELOG_ERROR(Fmt, Args),
        gmt_elog_policy:enabled(error, undefined, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_WARNING(Fmt, Args),
        gmt_elog_policy:enabled(warning, undefined, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_INFO(Fmt, Args),
        gmt_elog_policy:enabled(info, undefined, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_DEBUG(Fmt, Args),
        gmt_elog_policy:enabled(debug, undefined, ?MODULE, ?LINE, Fmt, Args)).

%%
%% Events with a category, a format, and args
%%

-define(ELOG_ERROR(Cat, Fmt, Args),
        gmt_elog_policy:enabled(error, Cat, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_WARNING(Cat, Fmt, Args),
        gmt_elog_policy:enabled(warning, Cat, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_INFO(Cat, Fmt, Args),
        gmt_elog_policy:enabled(info, Cat, ?MODULE, ?LINE, Fmt, Args)).
-define(ELOG_DEBUG(Cat, Fmt, Args),
        gmt_elog_policy:enabled(debug, Cat, ?MODULE, ?LINE, Fmt, Args)).

-endif. % -ifndef(gmt_elog_hrl).
