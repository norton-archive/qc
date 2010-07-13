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
%%% File    : gmt_event_h.hrl
%%% Purpose : GMT event handler header
%%%----------------------------------------------------------------------

-ifndef(gmt_event_h).
-define(gmt_event_h, true).

-include("gmt_applog.hrl").

%% Gemini only uses the following four levels.
-define(PSS_LOG_ALERT,   1).
-define(PSS_LOG_WARNING, 4).
-define(PSS_LOG_INFO,    6).
-define(PSS_LOG_DEBUG,   7).

-define(CODE7_DEFAULT, ?APPLOG_999).

%% Maximum depth for formatting "~P" and "~P" strings.
-define(FORMAT_MAXDEPTH, 35).

%% Error logger communication between gmt_event_h and whatever process
%% is interested in getting copies of the events

-record(gmt_event_h_copy, {
          now,                                  % Event time as now()
          date,                                 % Event time as {Date, Time}
          date_str,                             % Event time formatted string()
          os_pid,                               % OS pid string()
          module,                               % string()
          severity,                             % string()
          syslog_facility,                      % integer()
          code7,                                % 7-digit app code integer()
          event_str,                            % Formatted event string()
          raw_event                             % Raw event_handler event
         }).

-endif. % -ifndef(gmt_event_h)
