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
%%% File    : qc_impl.hrl
%%% Purpose : Wrapper for QuickCheck and Proper Implementations
%%%-------------------------------------------------------------------

-ifndef(qc_impl).
-define(qc_impl, true).

-ifdef(QC_PROPER).
-undef(PROPER).
-define(PROPER, true).
-undef(EQC).
-undef(TRIQ).
-endif.

-ifdef(QC_TRIQ).
-undef(TRIQ).
-define(TRIQ, true).
-undef(EQC).
-undef(PROPER).
-endif.

-ifdef(QC_EQC).
-undef(EQC).
-define(EQC, true).
-undef(PROPER).
-undef(TRIQ).
-endif.

-ifdef(QC_EQCMINI).
-undef(EQC).
-define(EQC, true).
-undef(PROPER).
-undef(TRIQ).
-endif.

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-undef(QC).
-define(QC, proper).
-define(QC_GEN, proper_gen).
-define(ALWAYS(_N,PROP), PROP).
-define(SOMETIMES(_N,PROP), PROP).
-undef(EQC).
-undef(TRIQ).
-endif. %% -ifdef(PROPER).

-ifdef(TRIQ).
-include_lib("triq/include/triq.hrl").
-undef(QC).
-define(QC, triq).
-define(QC_GEN, triq_dom).
-define(ALWAYS(_N,PROP), PROP).
-define(SOMETIMES(_N,PROP), PROP).
-define(SHRINK(G,_GS), G).
-undef(EQC).
-undef(PROPER).
-endif. %% -ifdef(TRIQ).

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-undef(QC).
-define(QC, eqc).
-define(QC_GEN, eqc_gen).
-undef(PROPER).
-undef(TRIQ).
-endif. %% -ifdef(EQC).

-endif. %% -ifdef(qc_impl).
