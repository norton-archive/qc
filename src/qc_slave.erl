%%%-------------------------------------------------------------------
%%% Copyright (C) 2013-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
%%% Copyright (c) 2012 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : qc_slave.erl
%%% Purpose : Wrapper for slave nodes adapted from gproc_dist_tests.erl
%%%-------------------------------------------------------------------

%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%% @author Ulf Wiger <ulf.wiger@erlang-solutions.com>
%%

-module(qc_slave).

%% API
-export([start_slave/1, restart_slave/1, stop_slave/1]).
-export([start_slaves/1, restart_slaves/1, stop_slaves/1]).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_slave(Name) ->
    start(Name).

restart_slave(Name) ->
    restart(Name).

stop_slave(Name) ->
    stop(Name).

start_slaves(Names) when is_list(Names) ->
    [H|T] = Nodes = [start(N) || N <- Names],
    _ = [rpc:call(H, net_adm, ping, [N]) || N <- T],
    Nodes.

restart_slaves(Names) when is_list(Names) ->
    [H|T] = Nodes = [restart(N) || N <- Names],
    _ = [rpc:call(H, net_adm, ping, [N]) || N <- T],
    Nodes.

stop_slaves(Names) when is_list(Names) ->
    [stop(N) || N <- Names].


%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------

start(Name) ->
    ok = init(),
    Args = args(),
    {ok, Node} = slave:start(host(), Name, Args),
    Node.

restart(Name) ->
    ok = init(),
    Args = args(),
    case slave:start(host(), Name, Args) of
        {ok, Node} ->
            Node;
        {error, {already_running, Node}} ->
            Node
    end.

stop(Name) ->
    slave:stop(slave(Name)).

init() ->
    case node() of
        nonode@nohost ->
            _ = os:cmd("epmd -daemon"),
            {ok, _} = net_kernel:start([?MODULE, shortnames]),
            ok;
        _ ->
            ok
    end.

args() ->
    {Pa, Pz} = paths(),
    "-pa ./ -pz ../ebin" ++
        lists:flatten([[" -pa " ++ Path || Path <- Pa],
                       [" -pz " ++ Path || Path <- Pz]]).

paths() ->
    Path = code:get_path(),
    {ok, [[Root]]} = init:get_argument(root),
    {Pas, Rest} = lists:splitwith(fun(P) ->
                                          not lists:prefix(Root, P)
                                  end, Path),
    {_, Pzs} = lists:splitwith(fun(P) ->
                                       lists:prefix(Root, P)
                               end, Rest),
    {Pas, Pzs}.

host() ->
    [_Name, Host] = re:split(atom_to_list(node()), "@", [{return, list}]),
    list_to_atom(Host).

slave(Name) when is_atom(Name) ->
    slave(atom_to_list(Name));
slave(Name) when is_list(Name) ->
    list_to_atom(Name ++ "@" ++ atom_to_list(host())).
