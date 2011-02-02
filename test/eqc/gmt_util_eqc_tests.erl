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
%%% File    : gmt_util_eqc_tests.erl
%%% Purpose : GMT util QuickCheck tests
%%%-------------------------------------------------------------------

-module(gmt_util_eqc_tests).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

run_tests_() ->
    {timeout, 60, [fun() -> run_tests() end]}.

run_tests() ->
    eqc:module({numtests, 500}, ?MODULE).

run_tests(Tests) ->
    eqc:module({numtests, Tests}, ?MODULE).

string() ->
    list(char()).

%% property to check gmt_util:left_pad() function
%% Checks length and correct padding/strings
prop_left_pad() ->
    ?FORALL({Str, Len, Char}, {string(), int(), char()},
            begin
                S = gmt_util:left_pad(Str, Len, Char),
                StrLen = length(Str),
                Res = case Len > StrLen of
                          true ->
                              PrefixLen = Len - StrLen,
                              {HL, TL} = lists:split(PrefixLen, S),
                              length(S) == Len andalso HL == lists:duplicate(PrefixLen, Char) andalso TL == Str;
                          false ->
                              S == Str
                      end,
                ?WHENFAIL(io:format("Str:~w S:~w~n",[Str, S]), Res)
            end).

%% property to check gmt_util:right_pad() function
%% Checks length and correct padding/strings
prop_right_pad() ->
    ?FORALL({Str, Len, Char}, {string(), int(), char()},
            begin
                S = gmt_util:right_pad(Str, Len, Char),
                StrLen = length(Str),
                Res = case Len > StrLen of
                          true ->
                              SuffixLen = Len - StrLen,
                              {HL, TL} = lists:split(StrLen, S),
                              length(S) == Len andalso HL == Str andalso TL == lists:duplicate(SuffixLen, Char);
                          false ->
                              S == Str
                      end,
                ?WHENFAIL(io:format("Str:~w S:~w~n",[Str, S]), Res)
            end).

prop_list_unique_u() ->
    ?FORALL(L, list(int()),
            lists:sort(gmt_util:list_unique_u(L)) == lists:usort(L)).

-endif. %% -ifdef(EQC).
