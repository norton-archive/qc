%%%-------------------------------------------------------------------
%%% Copyright (C) 2013-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
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
%%% File    : qc_statem.erl
%%% Purpose : Wrapper for statem
%%%-------------------------------------------------------------------

-module(qc_statem).

-ifdef(QC).

-include("qc_statem.hrl").

-ifdef(QC_STATEM).

%% API
-export([qc_run/3]).
-export([qc_sample/2]).
-export([qc_prop/2]).
-export([qc_check/3]).
-export([qc_check_file/3]).

%% Interface Functions
-ifndef(old_callbacks).

-type call() :: {call, Mod::atom(), Fun::atom(), Args::list(term())}.
-type var() :: {var, integer()}.

-callback command(SymState::term()) -> Gen::term().
-callback initial_state() -> SymState::term().
-callback initial_state(Options::proplist()) -> SymState::term().
-callback next_state(SymState::term(), R::var(), C::call()) -> SymState::term().
-callback invariant(DynState::term()) -> boolean().
-callback precondition(SymState::term(), C::call()) -> boolean().
-callback postcondition(DynState::term(), C::call(), R::term()) -> boolean().
-callback init() -> ok.
-callback init(SymState::term()) -> ok.
-callback stop(SymState::term(), DynState::term() | undefined) -> ok.
-callback aggregate([{N::integer(), Call::term(), R::term(), DynState::term()}]) -> [term()].

-else. % -ifndef(old_callbacks).

-export([behaviour_info/1]).

%% Define the behaviour's required mods.
behaviour_info(callbacks) ->
    [{command,1}
    , {initial_state,0}
    , {initial_state,1}
    , {next_state,3}
    , {invariant,1}
    , {precondition,2}
    , {postcondition,3}
    , {init,0}
    , {init,1}
    , {stop,2}
    , {aggregate,1}
    ];
behaviour_info(_Other) ->
	undefined.

-endif. % -ifndef(old_callbacks).

%%%----------------------------------------------------------------------
%%% types and records
%%%----------------------------------------------------------------------
-type options()  :: [{name,string()} | cover | {cover,[module()]} | parallel | noshrink | {sometimes,pos_integer()} | {timeout,timeout()} | any()].
-type proplist() :: proplists:proplist().
-type filename() :: file:name().

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec qc_run(module(), non_neg_integer(), options()) -> boolean().
qc_run(Mod, NumTests, Opts) ->
    Name = proplists:get_value(name, Opts, name(Mod)),
    Cover = proplists:get_value(cover, Opts, false),
    if is_list(Cover) ->
            cover_init(Cover);
       Cover ->
            cover_init([Mod]);
       true ->
            ok
    end,
    try
        case proplists:get_bool(noshrink, Opts) of
            false ->
                ?QC:quickcheck(numtests(NumTests, qc_prop(Mod, Opts)));
            true ->
                ?QC:quickcheck(numtests(NumTests, noshrink(qc_prop(Mod, Opts))))
        end
    after
        if is_list(Cover) ->
                cover_stop(Cover, Name);
           Cover ->
                cover_stop([Mod], Name);
           true ->
                ok
        end
    end.

-spec qc_sample(module(), proplist()) -> any().
qc_sample(Mod, Opts) ->
    %% sample
    ?QC_GEN:sample(commands(Mod, Mod:initial_state(Opts))).

-spec qc_prop(module(), proplist()) -> any().
qc_prop(Mod, Opts) ->
    %% init
    Start = erlang:now(),
    ok = Mod:init(),
    %% loop
    Name = proplists:get_value(name, Opts, Mod),
    Parallel = proplists:get_bool(parallel, Opts),
    Sometimes = proplists:get_value(sometimes, Opts, 1),
    Timeout = proplists:get_value(timeout, Opts, 10000),
    qc_prop1(Mod, Opts, Start, Name, Parallel, Sometimes, Timeout).

-spec qc_check(module(), proplist(), term()) -> any().
qc_check(Mod, Opts, CounterExample) ->
    ?QC:check(qc_prop(Mod, Opts), CounterExample).

-spec qc_check_file(module(), proplist(), filename()) -> any().
qc_check_file(Mod, Opts, FileName) ->
    {ok, [CounterExample]} = file:consult(FileName),
    qc_check(Mod, Opts, CounterExample).

%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------
qc_prop1(Mod, Opts, Start, Name, false, _Sometimes, Timeout) ->
    S0 = Mod:initial_state(Opts),
    ?FORALL(Cmds, more_commands(3, commands(Mod, S0)),
            ?SOMETIMES(_Sometimes,
                       ?TIMEOUT(Timeout,
                                begin
                                    %% init
                                    ok = Mod:init(S0),

                                    %% run
                                    {H,S,Res} = run_commands(Mod, Cmds, Opts),

                                    %% history
                                    Fun = fun({Cmd,{State,Reply}},{N,Acc}) -> {N+1,[{N,Cmd,Reply,State}|Acc]} end,
                                    {_, RevCmdsH} = lists:foldl(Fun, {1,[]}, zip(tl(Cmds),H)),
                                    CmdsH = lists:reverse(RevCmdsH),

                                    %% invariant
                                    Invariant = Mod:invariant(S),

                                    %% whenfail
                                    ?WHENFAIL(qc_prop_sequential_whenfail(Mod, Opts, Start, Name, Cmds, CmdsH, S, Res, Invariant),
                                              aggregate(Mod:aggregate(CmdsH),
                                                        (ok =:= Mod:stop(S0, S) andalso
                                                         ok =:= Res andalso
                                                         Invariant)))
                                end)));
qc_prop1(Mod, Opts, Start, Name, true, _Sometimes, Timeout) ->
    %% Number of attempts to make each test case fail. When searching
    %% for a failing example, we run each test once. When searching
    %% for a way to shrink a test case, we run each candidate
    %% shrinking 100 times.
    S0 = Mod:initial_state(Opts),
    ?FORALL(_Attempts, ?SHRINK(1,[100]),
            ?FORALL(Cmds, more_commands(3, parallel_commands(Mod, S0)),
                    ?ALWAYS(_Attempts,
                            ?SOMETIMES(_Sometimes,
                                       ?TIMEOUT(Timeout,
                                                begin
                                                    %% init
                                                    ok = Mod:init(S0),

                                                    %% run
                                                    {H,HL,Res} = run_parallel_commands(Mod, Cmds, Opts),

                                                    %% whenfail
                                                    ?WHENFAIL(qc_prop_parallel_whenfail(Mod, Opts, Start, Name, Cmds, H, HL, Res),
                                                              aggregate(command_names(Cmds),
                                                                        (ok =:= Mod:stop(S0, undefined) andalso
                                                                         ok =:= Res)))
                                                end))))).

qc_prop_sequential_whenfail(_Mod, Opts, Start, Name, Cmds, CmdsH, S, Res, Invariant) ->
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
                    %% opts
                    io_lib:format("~nOPTS:~n\t~p.~n",[Opts]),
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
                    %% invariant
                    io_lib:format("~nINVARIANT:~n\t~p.~n",[Invariant]),
                    %% commands end
                    io_lib:format("~nCOUNTEREXAMPLE END: ~p~n~n",[FileName])
                   ]
                  ),
        %% counterexample
        io:format(FileIoDev,"~n~n%% ~s~n~n",[string:join(re:split(Output, "\r?\n", [{return,list}]), "\n%% ")]),
        io:format(FileIoDev,"~p.~n",[[Cmds]]),
        %% stderr
        io:format("~nCOUNTEREXAMPLE: ~p~n",[FileName])
    after
        counterexample_close(FileIoDev)
    end.

qc_prop_parallel_whenfail(_Mod, Opts, Start, Name, Cmds, H, HL, Res) ->
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
                    %% opts
                    io_lib:format("~nOPTS:~n\t~p.~n",[Opts]),
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
        io:format(FileIoDev,"~p.~n",[[Cmds]]),
        %% stderr
        io:format("~nCOUNTEREXAMPLE: ~p~n",[FileName])
    after
        counterexample_close(FileIoDev)
    end.

name(Mod) ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
    lists:flatten(io_lib:format("~w-~4..0B~2..0B~2..0B-~2..0B~2..0B~2..0B",
                                [Mod,Year,Month,Day,Hour,Minute,Second])).

cover_init(Mods) when is_list(Mods) ->
    Fun = fun(Mod) ->
                  _ = cover:reset(Mod),
                  {ok, _} = cover:compile_beam(Mod)
          end,
    lists:foreach(Fun, Mods).

cover_stop(Mods, Name) when is_list(Mods) ->
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

-endif. %% -ifdef(QC_STATEM).

-endif. %% -ifdef(QC).
