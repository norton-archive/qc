%%%-------------------------------------------------------------------
%%% Copyright (c) 2010-2012 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : qc_statem_impl.erl
%%% Purpose : Wrapper Implementation for statem
%%%-------------------------------------------------------------------

-module(qc_statem_impl, [MOD]).

-ifdef(QC).

-include("qc_impl.hrl").

%% API
-export([qc_run/2]).
-export([qc_sample/1]).
-export([qc_prop/1]).

%% eqc_statem Callbacks
-export([command/1, next_state/3, precondition/2, postcondition/3]).

%%%----------------------------------------------------------------------
%%% types and records
%%%----------------------------------------------------------------------

-type proplist() :: [atom() | {atom(), term()}].

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec qc_run(non_neg_integer(), [{name,string()} | cover | {cover,[module()]} | parallel | noshrink | {sometimes,pos_integer()} | {timeout,timeout()} | any()]) -> boolean().
qc_run(NumTests, Options) ->
    Name = proplists:get_value(name, Options, name()),
    Cover = proplists:get_value(cover, Options, false),
    if is_list(Cover) ->
            cover_setup(Cover);
       Cover ->
            cover_setup([MOD]);
       true ->
            ok
    end,
    try
        Options1 = [{name,Name}|proplists:delete(name, Options)],
        Options2 = proplists:delete(cover, Options1),
        case proplists:get_bool(noshrink, Options2) of
            false ->
                ?QC:quickcheck(numtests(NumTests, qc_prop(Options2)));
            true ->
                Options3 = proplists:delete(noshrink, Options2),
                ?QC:quickcheck(numtests(NumTests, noshrink(qc_prop(Options3))))
        end
    after
        if
            is_list(Cover) ->
                cover_teardown(Cover, Name);
            Cover ->
                cover_teardown([MOD], Name);
            true ->
                ok
        end
    end.

-spec qc_sample(proplist()) -> any().
qc_sample(Options) ->
    %% sample
    Params = [{mod,MOD},{options,Options}],
    ?QC_GEN:sample(?FORALL(Scenario,with_parameters(Params,scenario()),
                           ?LET(S0,initial_state(Scenario),
                                command(S0)))).

-spec qc_prop(proplist()) -> any().
qc_prop(Options) ->
    %% setup
    Start = erlang:now(),
    ok = setup(),

    %% loop
    Name = proplists:get_value(name, Options, MOD),
    Parallel = proplists:get_bool(parallel, Options),
    Sometimes = proplists:get_value(sometimes, Options, 1),
    Timeout = proplists:get_value(timeout, Options, 10000),
    NewOptions = proplists:delete(timeout, proplists:delete(sometimes, proplists:delete(parallel, proplists:delete(name, Options)))),
    Params = [{parallel,Parallel}, {mod,MOD}, {options,NewOptions}],
    ?FORALL(Scenario,with_parameters(Params,scenario()),
            ?LET(S0,with_parameters(Params,initial_state(Scenario)),
                 qc_prop1(Parallel, Start, Options, Name, Sometimes, Timeout, Scenario, Params, S0))).

%%%----------------------------------------------------------------------
%%% Callbacks - eqc_statem
%%%----------------------------------------------------------------------

%% scenario generator
scenario() ->
    MOD:scenario_gen().

%% command generator
command(S) ->
    MOD:command_gen(S).

%% initial state
initial_state(Scenario) ->
    MOD:initial_state(Scenario).

%% state is sane
state_is_sane(S) ->
    MOD:state_is_sane(S).

%% next state
next_state(S,R,C) ->
    MOD:next_state(S,R,C).

%% precondition
precondition(S,C) ->
    MOD:precondition(S,C).

%% postcondition
postcondition(S,C,R) ->
    MOD:postcondition(S,C,R).

%% setup
setup() ->
    MOD:setup().

%% setup
setup(Scenario) ->
    MOD:setup(Scenario).

%% teardown
teardown(Ref, State) ->
    MOD:teardown(Ref, State).

%% aggregate
aggregate(L) ->
    MOD:aggregate(L).

%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------
qc_prop1(false, Start, Options, Name, Sometimes, Timeout, Scenario, Params, S0) ->
    ?FORALL(Cmds, more_commands(3,commands(THIS,S0)),
            ?SOMETIMES(Sometimes,
                       ?TIMEOUT(Timeout,
                                begin
                                    %% setup
                                    {ok,TestRef} = setup(Scenario),

                                    %% run
                                    {H,S,Res} = run_commands(THIS,Cmds,Params),

                                    %% history
                                    Fun = fun({Cmd,{State,Reply}},{N,Acc}) -> {N+1,[{N,Cmd,Reply,State}|Acc]} end,
                                    {_, RevCmdsH} = lists:foldl(Fun, {1,[]}, zip(tl(Cmds),H)),
                                    CmdsH = lists:reverse(RevCmdsH),

                                    %% sane
                                    Sane = state_is_sane(S),

                                    %% whenfail
                                    ?WHENFAIL(qc_prop_sequential_whenfail(Start, Options, Name, Scenario, Cmds, CmdsH, S, Res, Sane),
                                              aggregate(aggregate(CmdsH),
                                                        (ok =:= Res
                                                         andalso Sane
                                                         %% teardown
                                                         andalso ok =:= teardown(TestRef,S))))
                                end)));
qc_prop1(true, Start, Options, Name, Sometimes, Timeout, Scenario, Params, S0) ->
    %% Number of attempts to make each test case fail. When searching
    %% for a failing example, we run each test once. When searching
    %% for a way to shrink a test case, we run each candidate
    %% shrinking 100 times.
    ?FORALL(Attempts,?SHRINK(1,[100]),
            ?FORALL(Cmds, parallel_commands(THIS,S0),
                    ?ALWAYS(Attempts,
                            ?SOMETIMES(Sometimes,
                                       ?TIMEOUT(Timeout,
                                                begin
                                                    %% setup
                                                    {ok,TestRef} = setup(Scenario),

                                                    %% run
                                                    {H,HL,Res} = run_parallel_commands(THIS,Cmds,Params),

                                                    %% whenfail
                                                    ?WHENFAIL(qc_prop_parallel_whenfail(Start, Options, Name, Scenario, Cmds, H, HL, Res),
                                                              aggregate(command_names(Cmds),
                                                                        (ok =:= Res
                                                                         %% teardown
                                                                         andalso ok =:= teardown(TestRef,undefined))))
                                                end))))).

qc_prop_sequential_whenfail(Start, Options, Name, Scenario, Cmds, CmdsH, S, Res, Sane) ->
    Now = erlang:now(),
    FileName = counterexample_filename(Name),
    FileIoDev = counterexample_open(FileName),
    try
        LenCmdsH = length(CmdsH),
        Output = lists:flatten(
                   [
                    %% commands start
                    io_lib:format("~nCOUNTEREXAMPLE START: ~p~n",[FileName]),
                    %% duration
                    io_lib:format("~nDURATION (secs):~n\t~p.~n",[erlang:round(timer:now_diff(Now,Start) / 1000000.0)]),
                    %% options
                    io_lib:format("~nOPTIONS:~n\t~p.~n",[Options]),
                    %% history
                    io_lib:format("~nHISTORY:", []),
                    _ = if
                            CmdsH == [] ->
                                io_lib:format("~n none~n", []);
                            true ->
                                [ io_lib:format("~n ~p/~p:~n\t Cmd:~n\t\t~p.~n\t Reply:~n\t\t~p.~n\t State:~n\t\t~p.~n",
                                                [N,LenCmdsH,Cmd,Reply,State])
                                  || {N,Cmd,Reply,State} <- CmdsH ]
                        end,
                    %% result
                    io_lib:format("~nRESULT:~n\t~p.~n",[Res]),
                    %% state
                    io_lib:format("~nSTATE:~n\t~p.~n",[S]),
                    %% state is sane
                    io_lib:format("~nSTATE IS SANE:~n\t~p.~n",[Sane]),
                    %% commands end
                    io_lib:format("~nCOUNTEREXAMPLE END: ~p~n~n",[FileName])
                   ]
                  ),
        %% counterexample
        io:format(FileIoDev,"~n~n%% ~s~n~n",[string:join(re:split(Output, "\r?\n", [{return,list}]), "\n%% ")]),
        io:format(FileIoDev,"~p.~n",[[Scenario,Cmds]]),
        %% stderr
        io:format("~nCOUNTEREXAMPLE: ~p~n",[FileName])
    after
        counterexample_close(FileIoDev)
    end.

qc_prop_parallel_whenfail(Start, Options, Name, Scenario, Cmds, H, HL, Res) ->
    Now = erlang:now(),
    FileName = counterexample_filename(Name),
    FileIoDev = counterexample_open(FileName),
    try
        Output = lists:flatten(
                   [
                    %% commands start
                    io_lib:format("~nCOUNTEREXAMPLE START: ~p~n",[FileName]),
                    %% duration
                    io_lib:format("~nDURATION (secs):~n\t~p.~n",[erlang:round(timer:now_diff(Now,Start) / 1000000.0)]),
                    %% options
                    io_lib:format("~nOPTIONS:~n\t~p.~n",[Options]),
                    %% history
                    io_lib:format("~nHISTORY:~n\t~p.~n", [H]),
                    %% history list
                    io_lib:format("~nHISTORY LIST:~n\t~p.~n", [HL]),
                    %% result
                    io_lib:format("~nRESULT:~n\t~p.~n",[Res]),
                    %% commands end
                    io_lib:format("~nCOUNTEREXAMPLE END: ~p~n~n",[FileName])
                   ]
                  ),
        %% counterexample
        io:format(FileIoDev,"~n~n%% ~s~n~n",[string:join(re:split(Output, "\r?\n", [{return,list}]), "\n%% ")]),
        io:format(FileIoDev,"~p.~n",[[Scenario,Cmds]]),
        %% stderr
        io:format("~nCOUNTEREXAMPLE: ~p~n",[FileName])
    after
        counterexample_close(FileIoDev)
    end.

name() ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
    lists:flatten(io_lib:format("~w-~4..0B~2..0B~2..0B-~2..0B~2..0B~2..0B",
                                [MOD,Year,Month,Day,Hour,Minute,Second])).

cover_setup(Mods) when is_list(Mods) ->
    Fun = fun(Mod) ->
                  _ = cover:reset(Mod),
                  {ok, _} = cover:compile_beam(Mod)
          end,
    lists:foreach(Fun, Mods).

cover_teardown(Mods, Name) when is_list(Mods) ->
    Fun = fun(Mod) ->
                  FileName = Name ++ "-cover-" ++ atom_to_list(Mod),
                  io:format("~nCOVER:~n\t~p.{txt,html}~n",[FileName]),
                  {ok, _} = cover:analyse_to_file(Mod, FileName ++ ".txt", []),
                  {ok, _} = cover:analyse_to_file(Mod, FileName ++ ".html", [html]),
                  _ = cover:reset(Mod)
          end,
    lists:foreach(Fun, Mods).

counterexample_filename(Name) when is_tuple(Name) ->
    counterexample_filename(io_lib:format("~w", [Name]));
counterexample_filename(Name) ->
    {Mega, Sec, Micro} = now(),
    lists:flatten(io_lib:format("~s-counterexample-~B-~B-~B.erl", [Name, Mega, Sec, Micro])).

counterexample_open(FileName) ->
    {ok, IoDev} = file:open(FileName, [write, exclusive]),
    IoDev.

counterexample_close(IoDev) ->
    ok = file:close(IoDev),
    ok.

-endif. %% -ifdef(QC).
