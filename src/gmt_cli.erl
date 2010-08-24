%%%----------------------------------------------------------------------
%%% Copyright (c) 2009-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_cli.erl
%%% Purpose : GMT command line interface
%%%----------------------------------------------------------------------

-module(gmt_cli).

-include("applog.hrl").

-define(LONG_TIMEOUT, 1800*1000).

-export([start_link/4, init/1, terminate/2]).
-export([server/2]).
-export([server_loop/1, server_listen_loop/2]). % For internal use only.

-export([show/2, set/2, reload/2]).             % common commands

-record(state, {                                % Individual session state
          sock,
          module,                               % implementation module
          hello,                                % hello string
          prompt                                % prompt string
         }).

start_link(Port,Module,Prompt,Hello) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [Port,Module,Prompt,Hello], []).

init([Port, Module, Prompt, Hello]) ->
    S = #state{module=gmt_util:atom_ify(Module), prompt=Prompt, hello=Hello},
    _Pid = spawn_link(?MODULE, server, [Port, S]),
    {ok, S}.


terminate(_Reason, State) ->
    State.                                      % Nothing must be done here.

server(Port, S) ->
    {ok,LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, false},
                                       {backlog, 50}, {reuseaddr, true},
                                       {nodelay, true}, {keepalive, false},
                                       {sndbuf, 64*1024}, {recbuf, 8*1024}]),
    server_listen_loop(LSock, S).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

server_listen_loop(LSock, S) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid = spawn_link(fun() -> server_loop0(S#state{sock = Sock}) end),
    gen_tcp:controlling_process(Sock, Pid),
    Pid ! go_for_it,
    ?MODULE:server_listen_loop(LSock, S).

server_loop0(S) ->
    receive
        go_for_it -> ok
    end,
    send_nl(S, S#state.hello),
    server_loop(S).

server_loop(S) ->
    send(S, "\r\n" ++ S#state.prompt ++ " "),
    Line = binary_to_list(gmt_util:chomp_bin(get_line(S))),
    Ws = string:tokens(Line, " "),
    try begin
            if Ws =/= [] ->
                    case hd(Ws) of
                        X when X =:= "exit"; X =:= "logout"; X =:= "quit"; X =:= "q" ->
                            send_nl(S, "Goodbye!"),
                            close_exit_normal(S);
                        X when X =:= "help"; X =:= "h"; X =:= "?" ->
                            server_loop(do_help(S));
                        Cmd ->
                            Module = case is_common(Ws) of
                                         true ->
                                             ?MODULE;
                                         _ ->
                                             S#state.module
                                     end,
                            Fun = gmt_util:atom_ify(Cmd),
                            Args = tl(Ws),
                            ?APPLOG_DEBUG("MFA=~p/~p/~p", [Module,Fun,Args]),
                            {ok, Message} = Module:Fun(Args, S),
                            send_nl(S, Message),
                            server_loop(S)
                    end;
               true ->
                    send_nl(S, [""]),
                    ?MODULE:server_loop(S)
            end
        end
    catch
        error:undef ->
            send_nl(S, ["ERROR: ",
                        io_lib:format("undefined command: ~p", [Line])]),
            server_loop(S);
          _X:Err ->
            send_nl(S, ["ERROR: ",
                        io_lib:format("~p:~p", [_X, Err])]),
            close_exit_normal(S)
    end.

is_common(["show", "nodes"|_]) ->
    true;
is_common(["show", "tables"|_]) ->
    true;
is_common(["show", "ubf"|_]) ->
    true;
is_common(["set", "loglevel"|_]) ->
    true;
is_common(["reload", "central"|_]) ->
    true;
is_common(_) ->
    false.

do_help(S) when S#state.module==undefined ->
    common_help(S);
do_help(S) ->
    common_help(S),
    case catch apply(S#state.module, help, []) of
        {ok, MessageList} ->
            [send_nl(S, L) || L <- MessageList],
            S;
        Err ->
            send_nl(S, ["ERROR: ",
                        io_lib:format("~p", [Err])]),
            close_exit_normal(S)
    end.

common_help(S) ->
    [send_nl(S, L)
     || L <- ["Usage:"
              , ""
              , "  show nodes <NODEID>            -- check if <NODEID> is active or not"
              , "  show nodes                     -- list all active mnesia nodes"
              , "  show tables <TABLEID>          -- show information for <TABLEID>"
              , "  show tables                    -- show information for all mnesia tables"
              , "  show ubf                       -- show number of ubf connections and its max"
              , ""
              , "  set loglevel ALERT|WARNG|INFO|DEBUG"
              , "                                 -- set level of application log"
              , ""
              , "  reload central                 -- reload central.conf"
              , "  exit|logout|q|quit             -- Exit CLI"
             ]],
    S.



close_exit_normal(S) ->
    catch gen_tcp:close(S#state.sock),
    exit(normal).

send(S, IoList) ->
    gen_tcp:send(S#state.sock, IoList).

send_nl(S, IoList) ->
    send(S, [IoList, "\r\n"]).

get_line(S) ->
    case gen_tcp:recv(S#state.sock, 0, ?LONG_TIMEOUT) of
        {ok, Line} -> Line;
        {error, _} -> catch (gen_tcp:close(S#state.sock)),
                      exit(normal)              % TODO allow cleanup?
    end.

%% common commands %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
show(["nodes"|WordList],_S) ->
    Nodes = mnesia:system_info(running_db_nodes),
    case WordList of
        [] ->
            %% show all nodes
            {ok, [ ["\n", atom_to_list(N)] || N <- Nodes]};
        _ ->
            case lists:member(list_to_atom(hd(WordList)), Nodes) of
                true ->
                    {ok, ["'", hd(WordList), "' exists."]};
                _ ->
                    {ok, ["'", hd(WordList), "' does not exist."]}
            end
    end;
show(["tables"|WordList],_S) ->
    case WordList of
        [] ->
            {ok, [print_table_headers(),
                  [show_table(T)||T <- cli_tables()]]};
        _ ->
            Table = list_to_atom(hd(WordList)),
            case catch mnesia:table_info(Table, all) of
                {'EXIT', _ } ->
                    {ok, ["'", hd(WordList), "' not found."]};
                _ ->
                    {ok, [print_table_headers(),
                          show_table(Table)]}
            end
    end;
show(["ubf"|_],_S) ->
    {ok, [show_ubf(Server) || Server <- ubf_servers()]};
show(Err,_S) ->
    {ok, io_lib:format("command not found: show ~p", [Err])}.


set(["loglevel"|WordList],_S) ->
    LogLevel = gmt_util:to_upper(hd(WordList)),
    case catch gmt_event_h:set_loglevel(LogLevel) of
        ok ->
            gmt_config_svr:set_config_value(application_app_log_level, LogLevel),
            {ok, "OK."};
        _Error ->
            ?APPLOG_DEBUG("set loglevel failed ~p/~p", [LogLevel, _Error]),
            {ok, io_lib:format("ERR: set loglevel failed ~p", [LogLevel])}
    end;
set(Err,_S) ->
    {ok, io_lib:format("command not found: set ~p", [Err])}.

reload(["central"|_],_S) ->
    case catch gmt_config_svr:reload_config() of
        ok ->
            {ok, "OK."};
        {error, Reason} ->
            {ok, io_lib:format("error: ~p", [Reason])};
        Err ->
            {ok, io_lib:format("failed: ~p", [Err])}
    end;
reload(Err,_S) ->
    {ok, io_lib:format("command not found: reload ~p", [Err])}.


%% internal %%%%
print_table_headers() ->
    [io_lib:format("\n~-41s|~-10s|~-12s|~-16s~n",
                   ["table name","records","words","(average words)"]),
     io_lib:format("~82c~n",[$-])].

show_table(Table) ->
    NumRecords = mnesia:table_info(Table, size),
    Words = mnesia:table_info(Table, memory),
    if
        NumRecords > 0 ->
            io_lib:format("~-41s|~10w|~12w|~16.2f~n",
                          [atom_to_list(Table),
                           NumRecords, Words, Words/NumRecords]);
        true ->
            io_lib:format("~-41s|~10w|~12w|~16s~n",
                          [atom_to_list(Table),
                           NumRecords, Words, "--"])
    end.

cli_tables() ->
    try
        mnesia:system_info(tables)
    catch
        _:_ ->
            []
    end.

ubf_servers() ->
    {ok, UBFServers} = gmt_config_svr:get_config_value(cli_ubf_servers, ""),
    [gmt_util:atom_ify(X) || X <- string:tokens(UBFServers," ")].
show_ubf(Server) ->
    case catch proc_socket_server:server_status(Server) of
        {Num, Max} when is_integer(Num) andalso is_integer(Max) ->
            io_lib:format("~p has ~p connections(max=~p)~n", [Server, Num, Max]);
        _Err ->
            ?APPLOG_DEBUG("show_ubf: server_status error ~p", [_Err]),
            ""
    end.
