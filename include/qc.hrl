%%%-------------------------------------------------------------------
%%% Copyright (C) 2013-2016 by Joseph Wayne Norton <norton@alum.mit.edu>
%%% Copyright (c) 2009-2012 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : qc.hrl
%%% Purpose : Wrapper for QuickCheck and Proper
%%%-------------------------------------------------------------------

-ifndef(qc).
-define(qc, true).

-include("qc_impl.hrl").

-import(qc_gen, [ulist/1]).

-ifdef(TRIQ).
-import(qc_gen, [largeint/0]).
-endif.

-endif. %% -ifdef(qc).

