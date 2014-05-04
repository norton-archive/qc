%%%-------------------------------------------------------------------
%%% Copyright (C) 2013-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
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
%%% File    : qc_statem_impl.hrl
%%% Purpose : Wrapper for QuickCheck and Proper Implementations
%%%-------------------------------------------------------------------

-ifndef(qc_statem_impl).
-define(qc_statem_impl, true).

-ifdef(QC_PROPER).
-define(QC_STATEM, true).
-endif.

-ifdef(QC_TRIQ).
-undef(QC_STATEM). %% @TODO check compatibility
-endif.

-ifdef(QC_EQC).
-define(QC_STATEM, true).
-endif.

-ifdef(QC_EQCMINI).
-undef(QC_STATEM).
-endif.

-ifdef(PROPER).
-endif. %% -ifdef(PROPER).

-ifdef(TRIQ).
-ifdef(QC_STATEM).
-include_lib("triq/include/triq_statem.hrl").
-endif. %% -ifdef(QC_STATEM).
-endif. %% -ifdef(TRIQ).

-ifdef(EQC).
-ifdef(QC_STATEM).
-include_lib("eqc/include/eqc_statem.hrl").
-endif. %% -ifdef(QC_STATEM).
-endif. %% -ifdef(EQC).

-endif. %% -ifdef(qc_statem_impl).
