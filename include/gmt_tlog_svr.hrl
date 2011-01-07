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
%%% File    : gmt_tlog_svr.hrl
%%% Purpose : transaction log server header
%%%----------------------------------------------------------------------

-ifndef(gmt_tlog_svr).
-define(gmt_tlog_svr, true).

-record(tlog_core, {
          pid=os:getpid()
          , threadid=erlang:self()
          , date=undefined
          , type=undefined
          , proto=undefined
          , status=undefined
          , client=undefined
          , duration=undefined
          , server=erlang:node()
          , trid=undefined
          , gtrid=undefined
          , yaguid=undefined
          , uraid=undefined
          , extras=[]
         }).

-record(tlog_http, {
          http_username=undefined
          , http_req_body_size=undefined
          , http_req_host=undefined
          , http_req_line=undefined
          , http_req_port=undefined
          , http_req_size=undefined
          , http_res_body_size=undefined
          , http_res_code=undefined
          , http_res_size=undefined
         }).

-endif.
