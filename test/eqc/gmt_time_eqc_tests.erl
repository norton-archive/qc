%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_time_eqc_tests.erl
%%% Purpose : GMT time QuickCheck tests
%%%-------------------------------------------------------------------

-module(gmt_time_eqc_tests).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(GMTQC, proper).
-undef(EQC).
-endif. %% -ifdef(PROPER).

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(GMTQC, eqc).
-undef(PROPER).
-endif. %% -ifdef(EQC).

-ifdef(GMTQC).

-export([run/0]).
-compile(export_all).

%% run from eunit
eunit_test_() ->
    gmt_eqc:eunit_module(?MODULE, 3000).

run() ->
    run(3000).

run(Num) ->
    gmt_eqc:module({numtests,Num}, ?MODULE).

dayinmonth(Y,M) ->
    NumDays = calendar:last_day_of_the_month(Y, M),
    choose(1, NumDays).

divisible(N,M) ->
    N rem M == 0.

calendar_time() ->
    {choose(0,23),choose(0,59),choose(0,59)}.

calendar_date() ->
    calendar_date(2100).

calendar_date(YYYY)
  when YYYY >= 1970 ->
    ?LET(Y,choose(1970,YYYY),
         ?LET(M,choose(1,12),{Y,M,dayinmonth(Y,M)})).

calendar_date_unix() ->
    calendar_date(2037).

time_t() ->
    ?LET(DateTime,{calendar_date_unix(),calendar_time()},
         begin
             calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200
         end).

%% @todo investigate why this property fails when YYYY is greater than
%% 2037?
prop_time_t() ->
    ?FORALL(T1,time_t(),
            begin
                T2 = gmt_time:time_t_to_rfc2822(T1),
                T3 = gmt_time:rfc2822_to_time_t(T2),
                Res = T1 == T3,
                ?WHENFAIL(io:format("T1=~p T2=~p T3=~p~n", [T1, T2, T3]), Res)
            end
           ).

-endif. %% -ifdef(GMTQC).
