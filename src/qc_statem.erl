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
%%% File    : qc_statem.erl
%%% Purpose : Wrapper for statem
%%%-------------------------------------------------------------------

-module(qc_statem).

-ifdef(QC).

-include("qc_impl.hrl").

%% API
-export([qc_run/3]).
-export([qc_sample/2]).
-export([qc_prop/2]).
-export([qc_counterexample/3]).
-export([qc_counterexample_read/3]).
-export([qc_counterexample_write/2]).

%% eqc_statem Callbacks
-export([command/1, initial_state/0, initial_state/1, next_state/3, precondition/2, postcondition/3]).

%% Interface Functions
-export([behaviour_info/1]).

%% Define the behaviour's required mods.
behaviour_info(callbacks) ->
    [{command_gen,2}
     , {initial_state,0}
     , {state_is_sane,1}
     , {next_state,3}
     , {precondition,2}
     , {postcondition,3}
     , {setup,1}
     , {teardown,1}
     , {teardown,2}
     , {aggregate,1}
    ];
behaviour_info(_Other) ->
	undefined.

%%%----------------------------------------------------------------------
%%% types and records
%%%----------------------------------------------------------------------

-type proplist() :: [atom() | {atom(), term()}].

-opaque modstate() :: term().

-record(state, {mod :: module(), mod_state :: modstate()}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec qc_run(module(), non_neg_integer(), [{name,string()} | cover | {cover,[module()]} | parallel | noshrink | {sometimes,pos_integer()} | any()]) -> boolean().
qc_run(Mod, NumTests, Options) ->
    Name = proplists:get_value(name, Options, name(Mod)),
    Cover = proplists:get_value(cover, Options, false),
    if is_list(Cover) ->
            cover_setup(Cover);
       Cover ->
            cover_setup([Mod]);
       true ->
            ok
    end,
    try
        Options1 = [{name,Name}|proplists:delete(name, Options)],
        Options2 = proplists:delete(cover, Options1),
        case proplists:get_bool(noshrink, Options2) of
            false ->
                ?QC:quickcheck(numtests(NumTests, qc_prop(Mod, Options2)));
            true ->
                Options3 = proplists:delete(noshrink, Options2),
                ?QC:quickcheck(numtests(NumTests, noshrink(qc_prop(Mod, Options3))))
        end
    after
        if
            is_list(Cover) ->
                cover_teardown(Cover, Name);
            Cover ->
                cover_teardown([Mod], Name);
            true ->
                ok
        end
    end.

-spec qc_sample(module(), proplist()) -> any().
qc_sample(Mod, Options) ->
    %% sample
    Params = [{mod,Mod},{options,Options}],
    ?QC_GEN:sample(with_parameters(Params,
                                   ?LET(InitialState,initial_state(Mod),
                                        command(InitialState)))).

-spec qc_prop(module(), proplist()) -> any().
qc_prop(Mod, Options) ->
    %% setup and teardown
    Start = erlang:now(),
    {ok,TestRefOnce} = Mod:setup(true),
    ok = Mod:teardown(TestRefOnce),

    %% loop
    Name = proplists:get_value(name, Options, Mod),
    Parallel = proplists:get_bool(parallel, Options),
    Sometimes = proplists:get_value(sometimes, Options, 1),
    NewOptions = proplists:delete(sometimes, proplists:delete(parallel, proplists:delete(name, Options))),
    Params = [{parallel,Parallel}, {mod,Mod}, {options,NewOptions}],
    case Parallel of
        false ->
            ?FORALL(Cmds,with_parameters(Params,
                                         ?LET(InitialState,initial_state(Mod),
                                              more_commands(3,commands(?MODULE,InitialState)))),
                    ?SOMETIMES(Sometimes,
                               begin
                                   %% setup
                                   {ok,TestRef} = Mod:setup(false),

                                   %% run
                                   {H,S,Res} = run_commands(?MODULE,Cmds,Params),

                                   %% history
                                   Fun = fun({Cmd,{State,Reply}},{N,Acc}) -> {N+1,[{N,Cmd,Reply,State}|Acc]} end,
                                   {_, RevCmdsH} = lists:foldl(Fun, {1,[]}, zip(tl(Cmds),H)),
                                   CmdsH = lists:reverse(RevCmdsH),

                                   %% sane
                                   Sane = state_is_sane(Mod, S),

                                   %% whenfail
                                   ?WHENFAIL(
                                      begin
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
                                              io:format(FileIoDev,"~p.~n",[[Cmds]]),
                                              %% stderr
                                              io:format("~nCOUNTEREXAMPLE: ~p~n",[FileName])
                                          after
                                              counterexample_close(FileIoDev)
                                          end
                                      end,
                                      aggregate(Mod:aggregate(CmdsH),
                                                (ok =:= Res
                                                 andalso Sane
                                                 %% teardown
                                                 andalso ok =:= Mod:teardown(TestRef,S#state.mod_state))))
                               end));
        true ->
            %% Number of attempts to make each test case fail. When
            %% searching for a failing example, we run each test
            %% once. When searching for a way to shrink a test case,
            %% we run each candidate shrinking 100 times.
            ?FORALL(_Attempts,?SHRINK(1,[100]),
                    ?FORALL(Cmds,with_parameters(Params,
                                                 ?LET(InitialState,initial_state(Mod),
                                                      parallel_commands(?MODULE,InitialState))),
                            ?ALWAYS(_Attempts,
                                    ?TIMEOUT(5000,
                                             begin
                                                 %% setup
                                                 {ok,TestRef} = Mod:setup(false),

                                                 %% run
                                                 {H,HL,Res} = run_parallel_commands(?MODULE,Cmds,Params),

                                                 %% whenfail
                                                 ?WHENFAIL(
                                                    begin
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
                                                            io:format(FileIoDev,"~p.~n",[[Cmds]]),
                                                            %% stderr
                                                            io:format("~nCOUNTEREXAMPLE: ~p~n",[FileName])
                                                        after
                                                            counterexample_close(FileIoDev)
                                                        end
                                                    end,
                                                    aggregate(command_names(Cmds),
                                                              (ok =:= Res
                                                               %% teardown
                                                               andalso ok =:= Mod:teardown(TestRef,undefined))))
                                             end))))
    end.

qc_counterexample(Module, Options, CounterExample) ->
    ?QC:check(qc_prop(Module, Options), CounterExample).

qc_counterexample_read(Module, Options, FileName) ->
    {ok, [CounterExample]} = file:consult(FileName),
    qc_counterexample(Module, Options, CounterExample).

qc_counterexample_write(FileName, CounterExample) ->
    file:write_file(FileName, io_lib:format("~p.~n", [CounterExample])).

%%%----------------------------------------------------------------------
%%% Callbacks - eqc_statem
%%%----------------------------------------------------------------------

%% initial state
initial_state() ->
    #state{}.

initial_state(Mod) ->
    #state{mod=Mod, mod_state=Mod:initial_state()}.

%% state is sane
state_is_sane(Mod, S) ->
    Mod:state_is_sane(S#state.mod_state).

%% command generator
command(S)
  when is_record(S,state) ->
    (S#state.mod):command_gen(S#state.mod, S#state.mod_state).

%% next state
next_state(S,R,C) ->
    NewModState = (S#state.mod):next_state(S#state.mod_state,R,C),
    S#state{mod_state=NewModState}.

%% precondition
precondition(S,C) ->
    (S#state.mod):precondition(S#state.mod_state,C).

%% postcondition
postcondition(S,C,R) ->
    (S#state.mod):postcondition(S#state.mod_state,C,R).


%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------


%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------
name(Mod) ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
    lists:flatten(io_lib:format("~w-~4..0B~2..0B~2..0B-~2..0B~2..0B~2..0B",
                                [Mod,Year,Month,Day,Hour,Minute,Second])).

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
