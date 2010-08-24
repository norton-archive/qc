%%%----------------------------------------------------------------------
%%% Copyright (c) 2007-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_config_svr.erl
%%% Purpose : GMT config server
%%%----------------------------------------------------------------------

-module(gmt_config_svr).

-behaviour(gen_server).

-export([start_link/1
         , start_link/2
         , reload_config/0
         , reload_config/1
         , get_config_path/0
        ]).

-export([get_config_value/2, get_config_value/3
         , get_config_value_i/2, get_config_value_i/3
         , get_config_value_float/2, get_config_value_float/3
         , get_config_value_boolean/2, get_config_value_boolean/3
         , get_config_value_term/2, get_config_value_term/3
         , get_config_value_atom/2, get_config_value_atom/3
         , get_config_value_timeout/2, get_config_value_timeout/3
         , get_config_value_timeoutsec/2, get_config_value_timeoutsec/3
        ]).

-export([get_config_namespace_list/1
         , get_config_namespace_dict/1
         , get_config_value_from_dict/3
         , get_config_value_i_from_dict/3
         , get_config_value_float_from_dict/3
         , get_config_value_boolean_from_dict/3
         , get_config_value_term_from_dict/3
         , get_config_value_atom_from_dict/3
         , get_config_value_timeout_from_dict/3
         , get_config_value_timeoutsec_from_dict/3
        ]).

-export([set_config_value/2, set_config_value/3
        ]).


%% gen_server Callback Functions
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3
        ]).


%% TODO: Incomplete!  100% of public API is not yet covered.
-spec(get_config_value/2 :: (atom() | string(), term()) -> {ok, term()} ).
-spec(get_config_value_i/2 :: (atom() | string(), integer()) -> {ok, integer()} ).
-spec(set_config_value/2 :: (atom() | string(), term()) -> ok ).


%% config state item
-record(state, {
          file_name=undefined,
          kv_dict=undefined
         }).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Function: init(Args) ->
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([ConfigFile]) ->
    reload_config_file(#state{file_name=ConfigFile}).

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Function: handle_call(Request, From, State) ->
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(reload_config, _From, State) ->
    {ok, NewState} = reload_config_file(State),
    {reply, ok, NewState};

handle_call({reload_config, ConfigFile}, _From, State) ->
    {ok, NewState} = reload_config_file(State#state{file_name=ConfigFile}),
    {reply, ok, NewState};

handle_call(get_config_path, _From, #state{file_name=FileName}=State) ->
    {reply, FileName, State};

handle_call({get_config_value, Name, Namespace}, _From, #state{kv_dict=KvDict}=State) ->
    if
        Namespace =:= undefined ->
            Key = Name;
        true ->
            Key = {Namespace,Name}
    end,
    %% io:format("Key = ~w~n", [Key]),
    {reply, dict:find(Key, KvDict), State};

handle_call(get_dict, _From, #state{kv_dict=KvDict}=State) ->
    {reply, KvDict, State};

handle_call({get_config_namespace_list, Namespace}, _From, #state{kv_dict=KvDict}=State) ->
    Result = [ {K,V} || {{NS,K},V} <- dict:to_list(KvDict), NS =:= Namespace ],
    {reply, {ok, Result}, State};

handle_call({set_config_value, Name, Value, Namespace}, _From, #state{kv_dict=KvDict}=State) ->
    if
        Namespace =:= undefined ->
            Key = Name;
        true ->
            Key = {Namespace,Name}
    end,
    %% io:format("Key = ~w => ~w~n", [Key, Value]),
    {reply, ok, State#state{kv_dict=dict:store(Key, Value, KvDict)}}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Function: handle_case(Msg, State) ->
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(timeout, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    %% DEBUG ?APPLOG_DEBUG_MODULE(?MODULE, "terminate: top ~p", [Reason]),
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%----------------------------------------------------------------------
%%% Exported functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: start_link
%%----------------------------------------------------------------------
%% @spec (string()) -> {ok, Pid} | ignore | {error, Reason}
start_link(ConfigFile) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ConfigFile], []).

start_link(ConfigFile,Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [ConfigFile], []).


%%----------------------------------------------------------------------
%% Func: reload_config
%%----------------------------------------------------------------------
%% @spec () -> ok | {error, Reason}
reload_config() ->
    gen_server:call(?MODULE, reload_config).

%% @spec (ConfigFile) -> ok | {error, Reason}
reload_config(ConfigFile) ->
    gen_server:call(?MODULE, {reload_config, ConfigFile}).

%% @spec () -> ok | {error, Reason}
get_config_path() ->
    gen_server:call(?MODULE, get_config_path).


%%----------------------------------------------------------------------
%% Func: get_config_value
%%----------------------------------------------------------------------
%% @spec (atom() | string(), term()) -> {ok, string() | term()} | {error, Reason}
get_config_value(Name, Default) ->
    get_config_value(Name, Default, undefined).

%% @spec (atom(), term(), atom()) -> {ok, string() | term()} | {error, Reason}
get_config_value(Name, Default, Namespace)
  when is_atom(Name), is_atom(Namespace) ->
    case gen_server:call(?MODULE, {get_config_value, Name, Namespace}) of
        error ->
            {ok, Default};
        Else ->
            Else
    end;

%% @spec (atom(), term(), undefined) -> {ok, string() | term()} | {error, Reason}
get_config_value(Name, Default, undefined)
  when is_atom(Name) ->
    get_config_value(Name, Default, undefined);

%% @spec (string(), term(), undefined) -> {ok, string() | term()} | {error, Reason}
get_config_value(Name, Default, undefined)
  when is_list(Name) ->
    case parse_config_namespace(Name) of
        {Namespace,Name1} ->
            get_config_value(Name1, Default, Namespace);
        Name1 ->
            get_config_value(Name1, Default, undefined)
    end;

%% @spec (atom(), term(), string()) -> {ok, string() | term()} | {error, Reason}
get_config_value(Name, Default, Namespace)
  when is_atom(Name), is_list(Namespace) ->
    get_config_value(Name, Default, list_to_atom(Namespace));

%% @spec (string(), term(), atom()) -> {ok, string() | term()} | {error, Reason}
get_config_value(Name, Default, Namespace)
  when is_list(Name), is_atom(Namespace) ->
    get_config_value(list_to_atom(Name), Default, Namespace);

%% @spec (string(), term(), string()) -> {ok, string() | term()} | {error, Reason}
get_config_value(Name, Default, Namespace)
  when is_list(Name), is_list(Namespace) ->
    get_config_value(list_to_atom(Name), Default, list_to_atom(Namespace)).

%% @spec (atom() | string(), integer() | string()) -> {ok, integer()} | {error, Reason}
get_config_value_i(Name, Default) ->
    get_config_value_i(Name, Default, undefined).

%% @spec (atom() | string(), integer() | string(), atom() | string()) -> {ok, integer()} | {error, Reason}
get_config_value_i(Name, Default, Namespace) ->
    case get_config_value(Name, Default, Namespace) of
        {ok, Value} ->
            {ok, gmt_util:int_ify(Value)};
        Err ->
            Err
    end.

%% @spec (atom() | string(), integer() | string()) -> {ok, float()} | {error, Reason}
get_config_value_float(Name, Default) ->
    get_config_value_float(Name, Default, undefined).

%% @spec (atom() | string(), integer() | string(), atom() | string()) -> {ok, float()} | {error, Reason}
get_config_value_float(Name, Default, Namespace) ->
    case get_config_value(Name, Default, Namespace) of
        {ok, Value} ->
            {ok, gmt_util:float_ify(Value)};
        Err ->
            Err
    end.

%% @spec (atom() | string(), integer() | string()) -> {ok, bool()} | {error, Reason}
get_config_value_boolean(Name, Default) ->
    get_config_value_boolean(Name, Default, undefined).

%% @spec (atom() | string(), integer() | string(), atom() | string()) -> {ok, bool()} | {error, Reason}
get_config_value_boolean(Name, Default, Namespace) ->
    case get_config_value(Name, Default, Namespace) of
        {ok, Value} ->
            {ok, gmt_util:boolean_ify(Value)};
        Err ->
            Err
    end.

%% @spec (atom() | string(), term()) -> {ok, term()} | {error, string()}
get_config_value_term(Name, Default) ->
    get_config_value_term(Name, Default, undefined).

%% @spec (atom() | string(), term(), atom() | string()) -> {ok, term()} | {error, string()}
get_config_value_term(Name, Default, Namespace) ->
    case get_config_value(Name, Default, Namespace) of
        {ok, Default} ->
            {ok, Default};
        {ok, String} ->
            case erl_scan:string(String ++ ".") of
                {ok, Tokens, _} ->
                    case erl_parse:parse_term(Tokens) of
                        {ok, List} ->
                            {ok, List};
                        _ ->
                            {error, String}
                    end;
                _ ->
                    {error, String}
            end
    end.

%% @spec (atom() | string(), atom() | string()) -> {ok, atom()} | {error, Reason}
get_config_value_atom(Name, Default) ->
    get_config_value_atom(Name, Default, undefined).

%% @spec (atom() | string(), atom() | string(), atom() | string()) -> {ok, atom()} | {error, Reason}
get_config_value_atom(Name, Default, Namespace) ->
    case get_config_value(Name, Default, Namespace) of
        {ok, Value} ->
            {ok, gmt_util:atom_ify(Value)};
        Err ->
            Err
    end.

%% @spec (atom() | string(), integer() | string()) -> {ok, integer() | infinity} | {error, Reason}
get_config_value_timeout(Name, Default) ->
    get_config_value_timeout(Name, Default, undefined).

%% @spec (atom() | string(), integer() | string(), atom() | string()) -> {ok,  integer() | infinity} | {error, Reason}
get_config_value_timeout(Name, Default, Namespace) ->
    case get_config_value(Name, Default, Namespace) of
        {ok, Value} ->
            {ok, gmt_util:timeout_ify(Value)};
        Err ->
            Err
    end.

%% @spec (atom() | string(), integer() | string()) -> {ok, integer() | infinity} | {error, Reason}
get_config_value_timeoutsec(Name, Default) ->
    get_config_value_timeoutsec(Name, Default, undefined).

%% @spec (atom() | string(), integer() | string(), atom() | string()) -> {ok,  integer() | infinity} | {error, Reason}
get_config_value_timeoutsec(Name, Default, Namespace) ->
    case get_config_value(Name, Default, Namespace) of
        {ok, Value} ->
            {ok, gmt_util:timeoutsec_ify(Value)};
        Err ->
            Err
    end.


%%----------------------------------------------------------------------
%% Func: get_config_namespace_list
%%----------------------------------------------------------------------
%% @spec (atom()) -> {ok, list()} | {error, Reason}
get_config_namespace_list(Namespace) when is_atom(Namespace) ->
    gen_server:call(?MODULE, {get_config_namespace_list, Namespace});

%% @spec (string()) -> {ok, list()} | {error, Reason}
get_config_namespace_list(Namespace) when is_list(Namespace) ->
    get_config_namespace_list(list_to_atom(Namespace)).


%%----------------------------------------------------------------------
%% Func: get_config_namespace_dict
%%----------------------------------------------------------------------
%% @spec (atom()) -> {ok, dict()} | {error, Reason}
get_config_namespace_dict(Namespace) when is_atom(Namespace) ->
    case get_config_namespace_list(Namespace) of
        {ok, List} ->
            {ok, dict:from_list(List)};
        Err ->
            Err
    end;

%% @spec (string()) -> {ok, dict()} | {error, Reason}
get_config_namespace_dict(Namespace) when is_list(Namespace) ->
    get_config_namespace_dict(list_to_atom(Namespace)).


%%----------------------------------------------------------------------
%% Func: get_config_value_from_dict
%%----------------------------------------------------------------------
%% @spec (atom(), term(), dict()) -> {ok, string() | term()}
get_config_value_from_dict(Name, Default, Dict) when is_atom(Name) ->
    case dict:find(Name, Dict) of
        {ok, _Val}=Result ->
            Result;
        error ->
            {ok, Default}
    end;

%% @spec (string(), term(), dict()) -> {ok, string() | term()}
get_config_value_from_dict(Name, Default, Dict) when is_list(Name) ->
    get_config_value_from_dict(list_to_atom(Name), Default, Dict).

%% @spec (atom() | string(), integer() | string(), dict()) -> {ok, integer()} | {error, Reason}
get_config_value_i_from_dict(Name, Default, Dict) ->
    {ok, Value} = get_config_value_from_dict(Name, Default, Dict),
    {ok, gmt_util:int_ify(Value)}.

%% @spec (atom() | string(), integer() | string(), dict()) -> {ok, float()} | {error, Reason}
get_config_value_float_from_dict(Name, Default, Dict) ->
    {ok, Value} = get_config_value_from_dict(Name, Default, Dict),
    {ok, gmt_util:float_ify(Value)}.

%% @spec (atom() | string(), integer() | string(), dict()) -> {ok, bool()} | {error, Reason}
get_config_value_boolean_from_dict(Name, Default, Dict) ->
    {ok, Value} = get_config_value_from_dict(Name, Default, Dict),
    {ok, gmt_util:boolean_ify(Value)}.

%% @spec (atom(), term(), dict()) -> {ok, string()}
get_config_value_term_from_dict(Name, Default, Dict) ->
    case dict:find(Name, Dict) of
        {ok, String} ->
            case erl_scan:string(String ++ ".") of
                {ok, Tokens, _} ->
                    case erl_parse:parse_term(Tokens) of
                        {ok, _List}=Result ->
                            Result;
                        _ ->
                            {error, String}
                    end;
                _ ->
                    {error, String}
            end;
        error ->
            {ok, Default}
    end.

%% @spec (atom() | string(), integer() | string(), dict()) -> {ok, atom()} | {error, Reason}
get_config_value_atom_from_dict(Name, Default, Dict) ->
    {ok, Value} = get_config_value_from_dict(Name, Default, Dict),
    {ok, gmt_util:atom_ify(Value)}.

%% @spec (atom(), string(), dict()) -> {ok, string()}
get_config_value_timeout_from_dict(Name, Default, Dict) ->
    {ok, Value} = get_config_value_from_dict(Name, Default, Dict),
    {ok, gmt_util:timeout_ify(Value)}.

%% @spec (atom(), string(), dict()) -> {ok, string()}
get_config_value_timeoutsec_from_dict(Name, Default, Dict) ->
    {ok, Value} = get_config_value_from_dict(Name, Default, Dict),
    {ok, gmt_util:timeoutsec_ify(Value)}.


%%----------------------------------------------------------------------
%% Func: set_config_value
%%----------------------------------------------------------------------
%% @spec (atom() | list(), term()) -> {ok, string()} | {error, Reason}
set_config_value(Name, Value) ->
    set_config_value(Name, gmt_util:list_ify(Value), undefined).

%% @spec (atom(), term(), atom()) -> ok | {error, Reason}
set_config_value(Name, Value, Namespace)
  when is_atom(Name), is_atom(Namespace) ->
    gen_server:call(?MODULE, {set_config_value, Name, gmt_util:list_ify(Value), Namespace});

%% @spec (atom(), term(), undefined) -> ok | {error, Reason}
set_config_value(Name, Value, undefined)
  when is_atom(Name) ->
    set_config_value(Name, Value, undefined);

%% @spec (string(), term(), undefined) -> ok | {error, Reason}
set_config_value(Name, Value, undefined)
  when is_list(Name) ->
    case parse_config_namespace(Name) of
        {Namespace,Name1} ->
            set_config_value(Name1, Value, Namespace);
        Name1 ->
            set_config_value(Name1, Value, undefined)
    end;

%% @spec (atom(), term(), string()) -> ok | {error, Reason}
set_config_value(Name, Value, Namespace)
  when is_atom(Name), is_list(Namespace) ->
    set_config_value(Name, Value, list_to_atom(Namespace));

%% @spec (string(), term(), string()) -> ok | {error, Reason}
set_config_value(Name, Value, Namespace)
  when is_list(Name), is_atom(Namespace) ->
    set_config_value(list_to_atom(Name), Value, Namespace);

%% @spec (string(), term(), string()) -> ok | {error, Reason}
set_config_value(Name, Value, Namespace)
  when is_list(Name), is_list(Namespace) ->
    set_config_value(list_to_atom(Name), Value, list_to_atom(Namespace)).


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: parse_config_file
%%----------------------------------------------------------------------

parse_config_file(Data, State) ->
    Lines = string:tokens(binary_to_list(Data), "\n"),
    KvList = lists:foldl(fun parse_config_fun/2, [], Lines),
    KvDict = dict:from_list(KvList),
    {ok, State#state{kv_dict=KvDict}}.

%%----------------------------------------------------------------------
%% Func: parse_config_fun
%%----------------------------------------------------------------------

parse_config_fun([], Acc) ->
    Acc;
parse_config_fun([$# | _], Acc) ->
    Acc;
parse_config_fun(Line, Acc) ->
    case string:str(Line, ":") of
        N when N < 2 ->
            Acc;
        P ->
            Left = string:substr(Line, 1, P - 1),
            Right = string:strip(string:substr(Line, P + 1)),
            case parse_config_namespace(Left) of
                {_Namespace,_Name}=Left1 ->
                    Left1;
                Left1 ->
                    Left1
            end,
            [{Left1, Right} | Acc]
    end.

%%----------------------------------------------------------------------
%% Func: parse_config_namespace
%%----------------------------------------------------------------------

parse_config_namespace(Name) ->
    case string:str(Name, ".") of
        N when N < 2 ->
            list_to_atom(Name);
        P ->
            Namespace = string:substr(Name, 1, P - 1),
            Name1 = string:strip(string:substr(Name, P + 1)),
            {list_to_atom(Namespace),list_to_atom(Name1)}
    end.


%%----------------------------------------------------------------------
%% Func: reload_config_file
%%----------------------------------------------------------------------
reload_config_file(#state{file_name=FileName}=State) ->
    %% io:format("file_name = ~s~n", [FileName]),
    {ok, Data} = file:read_file(FileName),
    parse_config_file(Data, State).
