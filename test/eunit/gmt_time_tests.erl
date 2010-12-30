%%%----------------------------------------------------------------------
%%% Copyright (c) 2008-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_time_tests.erl
%%% Purpose : GMT time test suite
%%%----------------------------------------------------------------------

-module(gmt_time_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, gmt_time). % Module Under Test (a.k.a. DUT)
-define(ATM, ?MODULE). % Automatic Test Module (a.k.a. ATE)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Test Cases
%%

start_2_000_test() ->
    %%
    %% Title:
    %%   global timestamp
    %%
    %% Description:
    %%   misc. global timestamp tests
    %%
    %% Method:
    %%   n/a
    %%

    T1 = ?MUT:global_timestamp(),
    T2 = ?MUT:global_timestamp(),

    %% equal
    true = (?MUT:global_timestamp_cmp(T1, T1) =:= 0),
    true = (?MUT:global_timestamp_cmp(T2, T2) =:= 0),
    %% less than
    true = (?MUT:global_timestamp_cmp(T1, T2) < 0),
    %% greater than
    true = (?MUT:global_timestamp_cmp(T2, T1) > 0),

    FakeT1 = {erlang:now(), 'a'},
    FakeT2 = {erlang:now(), 'b'},

    %% equal
    true = (?MUT:global_timestamp_cmp(FakeT1, FakeT1) =:= 0),
    true = (?MUT:global_timestamp_cmp(FakeT2, FakeT2) =:= 0),
    %% less than
    true = (?MUT:global_timestamp_cmp(FakeT1, FakeT2) < 0),
    %% greater than
    true = (?MUT:global_timestamp_cmp(FakeT2, FakeT1) > 0),

    ok.

start_3_000_test() ->
    %% Test smpp_to_abs, as defined in the SMPP V5.0 spec
    io:format("Enter test ~s:smpp_to_abs~n", [?MUT]),

    %% Test absolute time format with both + and - time difference (4.7.23.4)
    Plus =  "080426041126001+",
    Minus = "080426044126001-",

    Base = {{2008, 4, 26},{4, 26, 26}},
    Secs = calendar:datetime_to_gregorian_seconds(Base),

    PSec = ?MUT:smpp_to_abs(Plus),
    MSec = ?MUT:smpp_to_abs(Minus),

    Secs = PSec,
    Secs = MSec,

    %% Test relative time format (4.7.23.4)

    %% null case
    Null = ?MUT:smpp_to_abs("000101000000000R"),
    Now = ?MUT:time_t(),
    Now = Null,

    %% 26 seconds
    Null26 = ?MUT:smpp_to_abs("000101000026000R"),
    Now26 = ?MUT:time_t() + 26,
    Null26 = Now26,

    io:format("Leave test ~s:smpp_to_abs, all tests passed!~n", [?MUT]),

    ok.

start_4_000_test() ->
    %%
    %% Title:
    %%   symbolic deadline timestamps
    %%
    %% Description:
    %%   misc. symbolic deadline timestamp tests
    %%
    %% Method:
    %%   n/a
    %%

    Now = erlang:now(),

    %% make_timeout
    0 = ?MUT:make_timeout({0,0,0}),
    0 = ?MUT:make_timeout({0,0,999}),

    1 = ?MUT:make_timeout({0,0,1000}),
    1 = ?MUT:make_timeout({0,0,1001}),

    1000 = ?MUT:make_timeout({0,1,0}),
    1000 = ?MUT:make_timeout({0,1,999}),

    1001 = ?MUT:make_timeout({0,1,1000}),
    1001 = ?MUT:make_timeout({0,1,1001}),

    %% make expires
    0 = ?MUT:make_timeout(Now, ?MUT:make_expires(Now, {0,0,0})),
    0 = ?MUT:make_timeout(Now, ?MUT:make_expires(Now, {0,0,999})),

    1 = ?MUT:make_timeout(Now, ?MUT:make_expires(Now, {0,0,1000})),
    1 = ?MUT:make_timeout(Now, ?MUT:make_expires(Now, {0,0,1001})),

    1000 = ?MUT:make_timeout(Now, ?MUT:make_expires(Now, {0,1,0})),
    1000 = ?MUT:make_timeout(Now, ?MUT:make_expires(Now, {0,1,999})),

    1001 = ?MUT:make_timeout(Now, ?MUT:make_expires(Now, {0,1,1000})),
    1001 = ?MUT:make_timeout(Now, ?MUT:make_expires(Now, {0,1,1001})),

    ok.
