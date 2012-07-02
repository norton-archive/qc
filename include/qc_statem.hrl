%%%-------------------------------------------------------------------
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
%%% File    : qc_statem.hrl
%%% Purpose : Wrapper for QuickCheck and Proper State Machine
%%%-------------------------------------------------------------------

-ifndef(qc_statem).
-define(qc_statem, true).

%% API
-export([qc_run/0, qc_run/1, qc_run/2]).
-export([qc_sample/0, qc_sample/1, qc_prop/0, qc_prop/1]).
-export([qc_counterexample/0, qc_counterexample/1, qc_counterexample/2]).
-export([qc_counterexample_read/1, qc_counterexample_write/1, qc_counterexample_write/2]).

-include("qc.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec qc_run() -> boolean().
qc_run() ->
    qc_run(500).

-spec qc_run(non_neg_integer()) -> boolean().
qc_run(NumTests) ->
    qc_run(NumTests, []).

-spec qc_run(non_neg_integer(), [{name,string()} | cover | {cover,[module()]} | parallel | noshrink | {sometimes,pos_integer()} | any()]) -> boolean().
qc_run(NumTests, Options) ->
    Name = proplists:get_value(name, Options, name(?MODULE)),
    Cover = proplists:get_value(cover, Options, false),
    _ = if is_list(Cover) orelse Cover -> cover_setup(Cover); true -> ok end,
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
        _ = if is_list(Cover) orelse Cover -> cover_teardown(Cover, Name); true -> ok end
    end.

%% sample
qc_sample() ->
    qc_sample([]).

qc_sample(Options) ->
    qc_statem:qc_sample(?MODULE, Options).

%% prop
qc_prop() ->
    qc_prop([]).

qc_prop(Options) ->
    qc_statem:qc_prop(?MODULE, Options).

%% counterexample
qc_counterexample() ->
    qc_counterexample([]).

qc_counterexample(Options) ->
    qc_counterexample(Options, ?QC:counterexample()).

qc_counterexample(Options, CounterExample) ->
    ?QC:check(qc_prop(Options), CounterExample).

%% counterexample read
qc_counterexample_read(FileName) ->
    {ok, [CounterExample]} = file:consult(FileName),
    qc_counterexample([], CounterExample).

%% counterexample write
qc_counterexample_write(FileName) ->
    qc_counterexample_write(FileName, ?QC:counterexample()).

qc_counterexample_write(FileName, CounterExample) ->
    file:write_file(FileName, io_lib:format("~p.~n", [CounterExample])).


%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------
name(Mod) ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
    lists:flatten(io_lib:format("~s-~4..0B~2..0B~2..0B-~2..0B~2..0B~2..0B",
                                [Mod,Year,Month,Day,Hour,Minute,Second])).

cover_setup(true) ->
    cover_setup([?MODULE]);
cover_setup(Mods) when is_list(Mods) ->
    [ begin
          _ = cover:reset(Mod),
          {ok, _} = cover:compile_beam(Mod)
      end || Mod <- Mods ],
    ok.

cover_teardown(true, Name) ->
    cover_teardown([?MODULE], Name);
cover_teardown(Mods, Name) when is_list(Mods) ->
    [ begin
          FileName = Name ++ "-cover-" ++ atom_to_list(Mod),
          io:format("~nCOVER:~n\t~p.{txt,html}~n",[FileName]),
          {ok, _} = cover:analyse_to_file(Mod, FileName ++ ".txt", []),
          {ok, _} = cover:analyse_to_file(Mod, FileName ++ ".html", [html]),
          _ = cover:reset(Mod)
      end || Mod <- Mods ],
    ok.

-endif. %% -ifdef(qc_statem).
