%%%----------------------------------------------------------------------
%%% Copyright (c) 2006-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : applog.hrl
%%% Purpose : application log events
%%%----------------------------------------------------------------------

-include("gmt_event_h.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @doc GMT UTIL appm class
%%
-define(APPLOG_ERRORCODE_APPM(X),
        ?APPLOG_003 + ?APPLOG_CLASS_APPM + X).

%%
%% @doc cause      Unexpected FSM event
%% @doc effect     The FSM shutdown abnormally.
%% @doc action     Check logs and manually issue the 'recover_X <thisnode>'
%%                 CLI command where X is listed by CLI help. If the problem persists,
%%                 contact Technical Support.
%% @doc monitor    Yes
%%
-define(APPLOG_APPM_001, ?APPLOG_ERRORCODE_APPM(1)).

%%
%% @doc cause      Unexpected FSM event
%% @doc effect     The FSM shutdown abnormally.
%% @doc action     Check logs and manually issue the 'recover_X <thisnode>'
%%                 CLI command where X is listed by CLI help. If the problem persists,
%%                 contact Technical Support.
%% @doc monitor    Yes
%%
-define(APPLOG_APPM_002, ?APPLOG_ERRORCODE_APPM(2)).

%%
%% @doc cause      Unexpected deliver_confirmation FSM event
%% @doc effect     The FSM event is ignored.
%% @doc action     Check logs and the operation and retry policy of external nodes.
%%                 If the problem persists, contact Technical Support.
%% @doc monitor    Yes
%%
-define(APPLOG_APPM_003, ?APPLOG_ERRORCODE_APPM(3)).

%%
%% @doc cause      Unexpected deliver_response FSM event
%% @doc effect     The FSM event is ignored.
%% @doc action     Check logs and the operation and retry policy of external nodes.
%%                 If the problem persists, contact Technical Support.
%% @doc monitor    Yes
%%
-define(APPLOG_APPM_004, ?APPLOG_ERRORCODE_APPM(4)).

%%
%% @doc cause      Unexpected deliver_confirmation FSM event
%% @doc effect     The FSM event is ignored.
%% @doc action     Check logs and the operation and retry policy of external nodes.
%%                 If the problem persists, contact Technical Support.
%% @doc monitor    Yes
%%
-define(APPLOG_APPM_005, ?APPLOG_ERRORCODE_APPM(5)).

%%
%% @unused
%%
-define(APPLOG_APPM_006, ?APPLOG_ERRORCODE_APPM(6)).

%%
%% @see APPLOG_APPM_004
%%
-define(APPLOG_APPM_007, ?APPLOG_ERRORCODE_APPM(7)).

%%
%% @see APPLOG_APPM_005
%%
-define(APPLOG_APPM_008, ?APPLOG_ERRORCODE_APPM(8)).

%%
%% @unused
%%
-define(APPLOG_APPM_009, ?APPLOG_ERRORCODE_APPM(9)).

%%
%% @see APPLOG_APPM_004
%%
-define(APPLOG_APPM_010, ?APPLOG_ERRORCODE_APPM(10)).

%%
%% @see APPLOG_APPM_004
%%
-define(APPLOG_APPM_011, ?APPLOG_ERRORCODE_APPM(11)).

%%
%% @see APPLOG_APPM_005
%%
-define(APPLOG_APPM_012, ?APPLOG_ERRORCODE_APPM(12)).

%%
%% @unused
%%
-define(APPLOG_APPM_013, ?APPLOG_ERRORCODE_APPM(13)).

%%
%% @unused - RESERVED (moved to another application)
%%
-define(APPLOG_APPM_014, ?APPLOG_ERRORCODE_APPM(14)).

%%
%% @doc cause      Unexpected deliver_confirmation FSM event
%% @doc effect     The FSM event is ignored.
%% @doc action     Check logs and the operation and retry configuration parameters.
%%                 If the problem persists, contact Technical Support.
%% @doc monitor    Yes
-define(APPLOG_APPM_015, ?APPLOG_ERRORCODE_APPM(15)).

%%
%% @doc cause      Unexpected deliver_request FSM event
%% @doc effect     The FSM event is ignored.
%% @doc action     Check logs and the operation and retry configuration parameters.
%%                 If the problem persists, contact Technical Support.
%% @doc monitor    Yes
%%
-define(APPLOG_APPM_016, ?APPLOG_ERRORCODE_APPM(16)).

%%
%% @see APPLOG_APPM_016
%%
-define(APPLOG_APPM_017, ?APPLOG_ERRORCODE_APPM(17)).

%%
%% @see APPLOG_APPM_016
%%
-define(APPLOG_APPM_018, ?APPLOG_ERRORCODE_APPM(18)).

%%
%% @see APPLOG_APPM_016
%%
-define(APPLOG_APPM_019, ?APPLOG_ERRORCODE_APPM(19)).

%%
%% @unused - RESERVED (moved to another application)
%%
-define(APPLOG_APPM_020, ?APPLOG_ERRORCODE_APPM(20)).

%%
%% @unused - RESERVED (moved to another application)
%%
-define(APPLOG_APPM_021, ?APPLOG_ERRORCODE_APPM(21)).

%%
%% @unused - RESERVED (moved to another application)
%%
-define(APPLOG_APPM_022, ?APPLOG_ERRORCODE_APPM(22)).

%%
%% @unused - RESERVED (moved to another application)
%%
-define(APPLOG_APPM_023, ?APPLOG_ERRORCODE_APPM(23)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @doc GMT UTIL info class
%%
-define(APPLOG_ERRORCODE_INFO(X),
        ?APPLOG_003 + ?APPLOG_CLASS_INFO + X).

%%
%% @doc cause      Application startup
%% @doc effect     Application logging is enabled
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_001, ?APPLOG_ERRORCODE_INFO(1)).

%%
%% @unused - RESERVED (moved to another application)
%%
-define(APPLOG_INFO_002, ?APPLOG_ERRORCODE_INFO(2)).

%%
%% @doc cause      FSM shutdown timeout
%% @doc effect     The FSM shutdown normally.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_003, ?APPLOG_ERRORCODE_INFO(3)).

%%
%% @unused APPLOG_INFO_004
%%
-define(APPLOG_INFO_004, ?APPLOG_ERRORCODE_INFO(4)).

%%
%% @doc cause      Temporary error with FSM final event
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_005, ?APPLOG_ERRORCODE_INFO(5)).

%%
%% @doc cause      Permanent error with FSM final event
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_006, ?APPLOG_ERRORCODE_INFO(6)).

%%
%% @doc cause      Permanent address error with FSM final event
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_007, ?APPLOG_ERRORCODE_INFO(7)).

%%
%% @doc cause      deliver_response FSM event timeout
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_008, ?APPLOG_ERRORCODE_INFO(8)).

%%
%% @doc cause      deliver_confirmation FSM event timeout
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_009, ?APPLOG_ERRORCODE_INFO(9)).

%%
%% @doc cause      deliver_alarm_timeout FSM event timeout
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_010, ?APPLOG_ERRORCODE_INFO(10)).

%%
%% @doc cause      Temporary error with FSM alarm timeout event
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_011, ?APPLOG_ERRORCODE_INFO(11)).

%%
%% @doc cause      Permanent error with FSM alarm timeout event
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_012, ?APPLOG_ERRORCODE_INFO(12)).

%%
%% @doc cause      Permanent address error with FSM alarm timeout event
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_013, ?APPLOG_ERRORCODE_INFO(13)).

%%
%% @doc cause      deliver_alarm_event FSM event timeout
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_014, ?APPLOG_ERRORCODE_INFO(14)).

%%
%% @doc cause      Temporary error with FSM alarm event event
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_015, ?APPLOG_ERRORCODE_INFO(15)).

%%
%% @doc cause      Permanent error with FSM alarm event event
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_016, ?APPLOG_ERRORCODE_INFO(16)).

%%
%% @doc cause      Permanent address error with FSM alarm event event
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_017, ?APPLOG_ERRORCODE_INFO(17)).

%%
%% @doc cause      deliver_request FSM event timeout
%% @doc effect     Application specific.
%% @doc action     Informational Only.
%%
-define(APPLOG_INFO_018, ?APPLOG_ERRORCODE_INFO(18)).
