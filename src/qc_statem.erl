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
-export([qc_sample/1, qc_sample/2]).
-export([qc_prop/1, qc_prop/2]).
-export([qc_gen_command/2]).

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
    ].

%%%----------------------------------------------------------------------
%%% types and records
%%%----------------------------------------------------------------------

-type proplist() :: [atom() | {atom(), term()}].

-opaque modstate() :: term().

-record(state, {mod :: module(), mod_state :: modstate()}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec qc_sample(module()) -> any().
qc_sample(Mod) ->
    qc_sample(Mod, []).

-spec qc_sample(module(), proplist()) -> any().
qc_sample(Mod, Options)
  when is_atom(Mod), is_list(Options) ->
    %% sample
    Params = [{mod,Mod},{options,Options}],
    ?QC_GEN:sample(with_parameters(Params,
                                   ?LET(InitialState,initial_state(Mod),
                                        command(InitialState)))).

-spec qc_prop(module()) -> any().
qc_prop(Mod) ->
    qc_prop(Mod, []).

-spec qc_prop(module(), proplist()) -> any().
qc_prop(Mod, Options)
  when is_atom(Mod), is_list(Options) ->
    %% setup and teardown
    {ok,TestRefOnce} = Mod:setup(true),
    ok = Mod:teardown(TestRefOnce),

    %% loop
    Parallel = proplists:get_bool(parallel, Options),
    Sometimes = proplists:get_value(sometimes, Options, 1),
    Params = [{parallel,Parallel},
              {mod,Mod},
              {options,proplists:delete(sometimes, proplists:delete(parallel, Options))}],
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

                                   %% whenfail
                                   ?WHENFAIL(
                                      begin
                                          %% commands
                                          FileName = write_commands(Mod,Cmds),
                                          io:format("~nCOMMANDS:~n\t~p~n",[FileName]),
                                          %% history
                                          io:format("~nHISTORY:"),
                                          _ = if
                                                  length(CmdsH) < 1 ->
                                                      io:format(" none~n");
                                                  true ->
                                                      [ io:format("~n #~p:~n\tCmd: ~p~n\tReply: ~p~n\tState: ~p~n",
                                                                  [N,Cmd,Reply,State])
                                                        || {N,Cmd,Reply,State} <- CmdsH ]
                                              end,
                                          %% result
                                          io:format("~nRESULT:~n\t~p~n",[Res]),
                                          %% state
                                          io:format("~nSTATE:~n\t~p~n",[S]),
                                          %% state is sane
                                          io:format("~nSTATE IS SANE:~n\t~p~n",[state_is_sane(Mod, S)])
                                      end,
                                      aggregate(Mod:aggregate(CmdsH),
                                                (ok =:= Res
                                                 andalso state_is_sane(Mod, S)
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
                                                        %% commands
                                                        FileName = write_commands(Mod,Cmds),
                                                        io:format("~nCOMMANDS:~n\t~p~n",[FileName]),
                                                        %% history
                                                        io:format("~nHISTORY:~n\t~p~n",[H]),
                                                        %% history list
                                                        io:format("~nHISTORY LIST:~n\t~p~n",[HL]),
                                                        %% result
                                                        io:format("~nRESULT:~n\t~p~n",[Res])
                                                    end,
                                                    aggregate(command_names(Cmds),
                                                              (ok =:= Res
                                                               %% teardown
                                                               andalso ok =:= Mod:teardown(TestRef,undefined))))
                                             end))))
    end;
qc_prop(_Mod, _Options) ->
    exit(badarg).

-spec qc_gen_command(module(), modstate()) -> any().
qc_gen_command(Mod, ModState) ->
    Mod:command_gen(Mod, ModState).


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

write_commands(Module,Cmds) ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
    FileName = lists:flatten(io_lib:format("~s-~4..0B~2..0B~2..0B-~2..0B~2..0B~2..0B.erl",
                                           [Module,Year,Month,Day,Hour,Minute,Second])),
    write_commands(Module,Cmds,FileName).

write_commands(_Module,Cmds,FileName) ->
    ok = file:write_file(FileName, io_lib:format("[~p].", [Cmds])),
    FileName.

-endif. %% -ifdef(QC).
