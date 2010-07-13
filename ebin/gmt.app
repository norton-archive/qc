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
%%% File    : gmt.app
%%% Purpose : GMT application
%%%----------------------------------------------------------------------

{application, gmt,
 [
  {description, "GMT dummy application"},
  {vsn, "0.01"},
  {id, "GMT"},
  {modules, [
	     %% TODO: fill in this list, perhaps
            ]
  },
  {registered, [ ] },
  %% NOTE: do not list applications which are load-only!
  {applications, [ kernel, stdlib, sasl ] },
  {mod, {gmt_app, []} }
 ]
}.
