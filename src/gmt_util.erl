%%%----------------------------------------------------------------------
%%% Copyright (c) 2006-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_util.erl
%%% Purpose : GMT misc utilities
%%%----------------------------------------------------------------------

-module(gmt_util).

-define(TCP_OP_TIMEOUT, 30*1000).
-define(CRLF, "\r\n").

-export([left_pad/3, right_pad/3]).
-export([is_char/1, is_string/1]).
-export([atom_ify/1, atomic_fusion/1, list_type_ify/2, bin_ify/1, boolean_ify/1,
         int_ify/1, float_ify/1, list_ify/1, random_item/1, timeout_ify/1, timeoutsec_ify/1]).
-export([int_infinity_ify/1, int_infinity_decrement/1, int_infinity_compare/2]).
-export([cal_to_bignum/1, cal_to_bignumstr/0, cal_to_bignumstr/1]).
-export([msisdn_extern_to_intern/1, msisdn_intern_to_extern/1]).
-export([enum_char_to_int/2, int_to_enum_char/2]).
-export([get_attrib_pos/3]).
-export([make_edoc_documentation/0, make_edoc_documentation/1]).
-export([file_to_term/1]).
-export([node_localid/0, node_localid/1, node_localid_port/1, node_localid_port/2]).
-export([shut_down_nicely/1]).
-export([rfc2254_string_ize/1, convert_hex/1, ascii_hex_to_val/1]).
-export([prune_term/2, prune_term_test/0]).
-export([to_lower/1, to_upper/1, join/2]).
-export([split_bin_on_char/2]).
-export([chomp_bin/1]).
-export([get_alarms/0,
         set_alarm/2, set_alarm/3, set_alarm/4,
         clear_alarm/1, clear_alarm/2, clear_alarm/3,
         alarm_set_p/1, alarm_set_p/2]).
-export([make_monitor/1, make_monitor/2, unmake_monitor/1]).
-export([total_memory/0, total_megs/0]).
-export([list_chunkfoldl/5, list_chop/2, list_keypartition/2]).
-export([permutations/1, combinations/1]).
-export([url_encode/1, url_encode/2]).
-export([parse_erlang_single_expr/1]).
-export([find_base_directory/1]).
-export([wait_for_processes/1, wait_for_processes/2, wait_for_processes_loop/2]).
-export([io_list_len/1]).
-export([list_unique/1, list_unique_u/1]).
-export([proplist_unique/1, proplist_update/2]).
-export([keyfindIgnoreCase/3]).
-export([list_to_bytes/1]).
-export([global_reference/0,
         global_reference/1,
         global_reference_cmp/2,
         global_reference_less/2,
         global_reference_binary/0,
         global_reference_binary/1]).
-export([randomize_list/1, randomize_list/2]).
-export([proplists_int_copy/3]).
-export([dbgon/1, dbgon/2, dbgadd/1, dbgadd/2, dbgdel/1, dbgdel/2, dbgoff/0]).
-export([node_left/0, node_right/0, node_left/1, node_right/1]).

%% TODO: Incomplete!  100% of public API is not yet covered.

%% UNUSED -type gen_serverspec() :: pid() | atom() | {global, any()}.

-spec atom_ify(atom() | binary() | string()) -> atom().
-spec bin_ify(atom() | binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | []) | integer()) -> binary().
-spec boolean_ify(boolean() | binary() | list() | 0 | 1) -> boolean().
-spec cal_to_bignumstr() -> string().
-spec combinations(list()) -> [any()].
-spec get_attrib_pos(atom(), atom(), integer() | atom()) -> integer().
-spec int_ify(binary() | [byte()] | number()) -> integer().
-spec int_to_enum_char(integer(), [any()]) -> string().
-spec io_list_len(binary() | list() | non_neg_integer()) -> number().
-spec join(list(), list() | char()) -> list().
-spec left_pad(string(), integer(), char()) -> string().
-spec list_ify(atom() | binary() | list() | integer() | tuple() | pid() | reference()) -> string().
-spec list_unique(list()) -> list().
-spec list_unique_u(list()) -> list().
-spec make_monitor(pid() | {atom(), atom()}) -> 'error' | {'ok',reference()}.
-spec msisdn_intern_to_extern(atom() | binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | []) | integer()) -> binary().
-spec node_localid_port(integer()) -> integer().
-spec permutations(list()) -> list(list()).
-spec right_pad(string(), integer(), char()) -> string().
-spec set_alarm(any(), any()) -> 'ok'.
-spec set_alarm(any(), any(), any()) -> 'ok'.
-spec set_alarm(any(), any(), any(),list()) -> 'ok'.
-spec split_bin_on_char(binary(), integer()) -> [binary(),...].
-spec timeout_ify(binary() | string() | timeout()) -> timeout().
-spec timeoutsec_ify(binary() | string() | timeout()) -> timeout().


%% @spec url_encode(Str::string())
%%       -> string()
%% @doc URL encodeing of Str --- see 2.2 of RFC1738
url_encode(Str) -> % no reserved chars
    url_encode(Str, []).

%% @spec url_encode(Str::string(), Reserved::string())
%%       -> string()
%% @doc URL encodeing of Str --- see 2.2 of RFC1738
%% @todo tail call optimization?
url_encode([H|T], Reserved) when $a =< H, H =< $z -> % small alphas
    [H|url_encode(T, Reserved)];
url_encode([H|T], Reserved) when $A =< H, H =< $Z -> % big alphas
    [H|url_encode(T, Reserved)];
url_encode([H|T], Reserved) when $0 =< H, H =< $9 -> % nums
    [H|url_encode(T, Reserved)];
url_encode([H|T], Reserved) ->
    Specials = "$-_.+!*'(),",
    case lists:member(H, Reserved) of
        true -> [H|url_encode(T, Reserved)];         % Reserved
        false -> case lists:member(H, Specials) of
                     true -> [H|url_encode(T, Reserved)];  % Specials
                     false ->
                                                % otherwise encode this char
                         url_encode_char(H)++url_encode(T, Reserved)
                 end
    end;
url_encode([], _Reserved) ->
    [].

url_encode_char(Char) ->
    case erlang:integer_to_list(Char, 16) of
        [X, Y] -> [$%, X, Y];
        [X] -> [$%, $0, X]
    end.


%% @spec left_pad(Str::string(), Len::integer(), PadChar::char())
%%       -> string()
%% @doc Left-pad a string Str to length Len using PadChar.

left_pad(Str, Len, PadChar) ->
    L = length(Str),
    if L >= Len      -> Str;
       L + 1 =:= Len -> [PadChar|Str];
       L + 2 =:= Len -> [PadChar, PadChar|Str];
       L + 3 =:= Len -> [PadChar, PadChar, PadChar|Str];
       L + 4 =:= Len -> [PadChar, PadChar, PadChar, PadChar|Str];
       L < Len ->
            lists:duplicate(Len - L, PadChar) ++ Str
    end.

%% @spec right_pad(Str::string(), Len::integer(), PadChar::char())
%%       -> string()
%% @doc Right-pad a string Str to length Len using PadChar.

right_pad(Str, Len, PadChar) ->
    L = length(Str),
    if L < Len ->
            Str ++ lists:duplicate(Len - L, PadChar);
       true ->
            Str
    end.

%% @spec is_char(X::term()) -> bool()
%% @doc Check if X is a printable character

is_char(X) ->
    if X < 0 -> false;
       X > 255 -> false;
       true -> true
    end.

%% @spec is_string(X::term()) -> bool()
%% @doc Check if X is a list of printable characters

is_string(X) when not is_list(X) ->
    false;
is_string([]) ->
    true;
is_string([X | Xs]) ->
    is_char(X) andalso is_string(Xs).

%% @spec atom_ify(X::term()) -> atom()
%% @doc Convert a term to a atom (if convertable).

atom_ify(X) when is_atom(X) ->
    X;
atom_ify(X) when is_binary(X) ->
    list_to_atom(binary_to_list(X));
atom_ify(X) when is_list(X) ->
    list_to_atom(X).

%% @spec atomic_fusion(X::list()) -> atom()
%% @doc Convert a list of terms to a single fused atom (if convertable).

atomic_fusion(H) when is_list(H) ->
    H2 = list_type_ify(H, string),
    He3 = iolist_to_binary(H2),
    He4 = binary_to_list(He3),
    list_to_atom(He4).

%% @spec bin_ify(X::term()) -> binary()
%% @doc Convert a term to a binary (if convertable).

bin_ify(X) when is_binary(X) ->
    X;
bin_ify(X) when is_list(X) ->
    list_to_binary(X);
bin_ify(X) when is_integer(X) ->
    list_to_binary(integer_to_list(X));
bin_ify(X) when is_atom(X) ->
    %% QQQ This appears to be faster than list_to_binary(atom_to_list())
    %% every time.  Double-check using cprof and perhaps eprof.
    case get(X) of
        undefined -> B = list_to_binary(atom_to_list(X)),
                     put(X, B),
                     B;
        B when is_binary(B) -> B
    end.

%% @spec int_ify(X::term()) -> integer()
%% @doc Convert a term to an integer (if convertable).

int_ify(X) when is_integer(X) ->
    X;
int_ify(X) when is_float(X) ->
    trunc(X);
int_ify(X) when is_binary(X) ->
    list_to_integer(binary_to_list(X));
int_ify(X) when is_list(X) ->
    list_to_integer(X).

%% @spec float_ify(X::term()) -> float()
%% @doc Convert a term to an float (if convertable).

float_ify(X) when is_float(X) ->
    X;
float_ify(X) when is_binary(X) ->
    list_to_float(binary_to_list(X));
float_ify(X) when is_list(X) ->
    list_to_float(X).

%% @spec list_ify(X::term()) -> string()
%% @doc Convert a term to a string list (if convertable).

list_ify(X) when is_list(X) ->
    X;
list_ify(X) when is_integer(X) ->
    integer_to_list(X);
list_ify(X) when is_binary(X) ->
    binary_to_list(X);
list_ify(X) when is_atom(X) ->
    atom_to_list(X);
list_ify(X) when is_tuple(X) ->
    tuple_to_list(X);
list_ify(X) when is_pid(X) ->
    io_lib:format("~p", [X]);
list_ify(X) when is_reference(X) ->
    erlang:ref_to_list(X).

%% @spec (X::term()) -> bool()
%% @doc Convert a term to a boolean (if convertable)

boolean_ify(true) -> true;
boolean_ify(false) -> false;
boolean_ify("true") -> true;
boolean_ify("false") -> false;
boolean_ify(<<"true">>) -> true;
boolean_ify(<<"false">>) -> false;
boolean_ify(1) -> true;
boolean_ify(0) -> false;
boolean_ify("1") -> true;
boolean_ify("0") -> false;
boolean_ify(<<"1">>) -> true;
boolean_ify(<<"0">>) -> false.

%% @spec (L::list(), T::atom()) -> list()
%% @doc Return L if all members of L are of PSS type T or are convertable
%% to type T, else throw error.
%%
%% If conversion to type T is possible, conversion will be done, so the
%% return value may not be exactly equal to input argument L.
%%
%% So far, the valid types for T are: int, string, atom, and A where A
%% is a PSS record type.  Remember: PSS string = Erlang binary.

list_type_ify(L, T) when is_list(L) ->
    F = case T of
            int ->
                fun(X) when is_integer(X) -> X;
                   (X)                    -> int_ify(X)
                end;
            %% Next clause is fix for bug in iter fun in
            %% pssql_protocol:parse_to_match_spec_and() and coerce_type().
            string when hd(L) >= 0, hd(L) =< 255 ->
                fun(X) -> X end;
            string ->
                fun(X) when is_binary(X) -> X;
                   (X)                   -> bin_ify(X)
                end;
            atom ->
                fun(X) when is_atom(X) -> X;
                   (X)                   -> atom_ify(X)
                end;
            Type when is_atom(Type) ->
                fun(X) when is_tuple(X),
                            element(1, X) =:= T -> X;
                   (_)                   -> throw({error, list_type_ify, T, L})
                end
        end,
    lists:map(F, L).

%% @spec (integer() | string() | binary()) -> integer() | infinity
%% @doc Given a term, return infinity atom or an integer if convertible
int_infinity_ify(infinity) ->
    infinity;
int_infinity_ify("infinity") ->
    infinity;
int_infinity_ify(<<"infinity">>) ->
    infinity;
int_infinity_ify(X) ->
    int_ify(X).

%% @spec (integer() | infinity) -> integer() | infinity
%% @doc Given a Count, return Count-1
int_infinity_decrement(infinity) ->
    infinity;
int_infinity_decrement(Count) when is_integer(Count) ->
    Count - 1.

%% @spec (integer() | infinity, integer() | infinity) -> integer()
%% @doc Compare integers A and B. Return positive number if A is larger than
%% B, 0 if equal and a negitive number if A is less than B. Its not legal to
%% compare infinity to infinity. infinity is always larger than any integer.
int_infinity_compare(A, infinity) when is_integer(A) ->
    -1;
int_infinity_compare(infinity, B) when is_integer(B) ->
    1;
int_infinity_compare(A, B) when is_integer(A) and is_integer(B)->
    A - B.

%% @spec random_item(T::tuple()) -> term()
%% @doc Return a random member of tuple T.

random_item(T) when is_tuple(T) ->
    N = random:uniform(tuple_size(T) - 1) + 1,
    element(N, T).

%% @spec timeout_ify(X::term()) -> timeout()
%% @doc Convert a term to an integer or the atom 'infinity' (if convertable).

timeout_ify(infinity) ->
    infinity;
timeout_ify("infinity") ->
    infinity;
timeout_ify(<<"infinity">>) ->
    infinity;
timeout_ify(X) ->
    int_ify(X).

%% @spec timeoutsec_ify(X::term()) -> integer() | infinity
%% @doc Convert a term to an integer or the atom 'infinity' (if
%% convertable). Input value is in the units of seconds

timeoutsec_ify(Timeout) ->
    case timeout_ify(Timeout) of
        infinity ->
            infinity;
        0 ->
            0;
        Timeout1 ->
            timer:seconds(Timeout1)
    end.

cal_to_bignum({{Year, Mon, Day}, {Hour, Min, Sec}}) ->
    (((((((((Year * 100) + Mon) * 100) + Day) * 100) + Hour) * 100) + Min) * 100) + Sec.

%% @spec () -> string()
%% @equiv cal_to_bignum(calendar:local_time())

cal_to_bignumstr() ->
    cal_to_bignumstr(calendar:local_time()).

%% @spec (calendar_date()) -> string()
%% @doc Convert calendar module-style date &amp; time to a string.
%% The format of the string is: "YYYYMMDDHHMMSS".

cal_to_bignumstr(CalDate) ->
    N = cal_to_bignum(CalDate),
    left_pad(integer_to_list(N), 14, $0).

%% @spec (Extern::stringish()) -> msisdn_intern()
%% @doc Convert a string containing an MSISDN into internal form,
%% preserving an optional leading zero digit.
%% Using integer() internally saves a bit more RAM than using binary().

msisdn_extern_to_intern(Extern) ->
    Ext = bin_ify(Extern),              % Assume binary is usually given to us
    case Ext of
        <<N:8/integer, _/binary>> when N =:= $0 ->
            -1 * int_ify(Ext);
        _ ->
            int_ify(Ext)
    end.

%% @spec (Extern::msisdn_intern()) -> binary()
%% @doc Convert a string containing an MSISDN into external form,
%% preserving an optional leading zero digit.

msisdn_intern_to_extern(Intern) when Intern < 0 ->
    bin_ify([$0|list_ify(-1 * Intern)]);
msisdn_intern_to_extern(Intern) ->
    bin_ify(Intern).

%% @spec (V::stringish(), EList::enum_list()) -> integer()
%% @doc Convert an enum char (typically 1 or 2 bytes) to a single integer.
%% <p>An enum_list() member can be an integer() (i.e. an ASCII character valid
%% in the enum), or it can be a {string(), integer()} combination, where
%% the string() is a multi-byte enum (e.g. "AB" as a bloodtype) and the
%% integer() is the value we're mapping it to.</p>
%% <p>If the value V isn't found in EList, throw an error.
%% </p>

enum_char_to_int(V, EList) ->
    case list_ify(V) of
        [C] ->                                  % Single char string
            case lists:member(C, EList) of
                true -> C;
                _    -> throw({bad_enum, list_ify(V), EList})
            end;
        S ->                                    % Multi char string
            case lists:keyfind(S, 1, EList) of
                {S, Int} -> Int;
                _   -> throw({bad_enum, list_ify(V), EList})
            end
    end.

%% @spec (Int::integer(), EList::enum_list()) -> string() | error
%% @doc Convert an integer to an enum char (typically 1 or 2 bytes).
%% @see enum_char_to_int/2.
int_to_enum_char(Int, EList) ->
    case lists:member(Int, EList) of
        true -> [Int];                          % Char -> string
        _    -> case lists:keyfind(Int, 2, EList) of
                    {Str, Int} -> Str;
                    _          -> throw({bad_enum, Int, EList})
                end
    end.


%% @spec (Mod::atom(), Table::atom(), integer() | atom()) -> integer()
%% @doc Return the tuple element number of the Attrib attribute in Table.

get_attrib_pos(_Mod, _Table, Attrib) when is_integer(Attrib) ->
    Attrib;
get_attrib_pos(Mod, Table, Attrib) ->
    FmtList = Mod:get_rec_fmtlist(Table),
    case lists:keyfind(Attrib, 1, FmtList) of
        {_, Pos, _} ->
            Pos;
        _ ->
            throw({error, get_attrib_pos,
                   unknown_attribute, Table, Attrib})
    end.

%% @spec () -> ok | whatever
%% @doc Helper function to make it easier to generate Edoc documentation
%%
%% We assume that the current working directory is the app's "src"
%% dir, so if we go up 1 level to find the name of the app's dir,
%% e.g. "foo-app__HEAD", then up one more, <tt>edoc:application()</tt>
%% will work correctly.

make_edoc_documentation() ->
    make_edoc_documentation([undefined]).

%% @spec ([atom()]) -> ok | whatever
%% @doc Helper function to make it easier to generate Edoc documentation
%%
%% We assume that the current working directory is the app's "src"
%% dir, so if we go up 1 level to find the name of the app's dir,
%% e.g. "foo-app__HEAD", then up one more, <tt>edoc:application()</tt>
%% will work correctly.

make_edoc_documentation([ApplicationName]) ->
    {ok, CurrentDir} = file:get_cwd(),
    ok = file:set_cwd(".."),
    {ok, Up1Path} = file:get_cwd(),
    LastDir = filename:basename(Up1Path),
    ok = file:set_cwd(".."),
    Options = [{new, true}, {hidden, true}, {private, true}, {todo, true}],
    Res =
        if ApplicationName =/= undefined ->
                edoc:application(atom_ify(ApplicationName), LastDir, Options);
           true ->
                edoc:application(atom_ify(LastDir), LastDir, Options)
        end,

    ok = file:set_cwd(CurrentDir),
    Res.

%% @spec (File::filename()) -> {ok, term()} | {error, term()}
%% @doc Helper function to read an Erlang term from a file and convert
%% it to its internal format for manipulation by other Erlang functions.

file_to_term(File) ->
    case file:read_file(File) of
        {ok, C} ->
            case erl_scan:string(binary_to_list(C)) of
                {ok, Ts, _} ->
                    erl_parse:parse_term(Ts);
                {error, X, Y} ->
                    {error, {X, Y}}
            end;
        Err ->
            Err
    end.

%% @spec () -> integer()
%% @doc Determine node's local id based on node's sname

node_localid() ->
    case init:get_argument(sname) of
        {ok, [[SName]]} -> node_localid(SName);
        _ -> 0
    end.

%% @spec (string()) -> integer()
%% @doc Determine node's local id based on node's sname

node_localid(SName) ->
    case re:run(SName, "_[0-9]+$") of
        {match,[]} ->
            0;
        {match,[{Start,Length}]} ->
            int_ify(string:substr(SName, Start+1, Length))
    end.

%% @spec (integer()) -> integer()
%% @doc Determine node's local port based on node's local id

node_localid_port(PortBase) ->
    node_localid_port(PortBase, node_localid()).

%% @spec (integer(), integer()) -> integer()
%% @doc Determine node's local port based on node's local id

node_localid_port(PortBase, LocalId) ->
    PortBase + (LocalId * 100).

%% @spec (list()) -> no_return_value
%% @doc Shutdown one or more Erlang nodes.
%% erlang:halt/1 is called unconditionally: the exit status will
%% be zero if all nodes are shut down successfully, non-zero for
%% each shutdown attempt that failed.

shut_down_nicely([Node|AppList]) ->
    Fstop = fun() ->
                    rpc:call(Node, init, stop, []),
                    0
            end,
    Fmatch = fun({Pattern, F}, Acc) ->
                     case catch F() of
                         Pattern -> Acc;
                         %%_X       -> io:format("ERR: got ~p, expected ~p\n", [_X, Pattern]), Acc + 1
                         _       -> Acc + 1
                     end
             end,
    Fisdown = fun(_, pang = Acc) ->
                      Acc;
                 (_, pong) ->
                      timer:sleep(250),
                      net_adm:ping(Node)
              end,

    AppArgs = [{ok, fun() -> rpc:call(Node, application, stop, [App]) end} ||
                  App <- AppList],
    Exit =
        Fmatch({pong,
                fun() -> net_adm:ping(Node) end}, 0) +
        lists:foldl(Fmatch, 0, AppArgs) +
        Fstop() +
        Fmatch({pang,
                fun() -> lists:foldl(Fisdown, pong, lists:seq(1, 12)) end}, 0),
    erlang:halt(Exit).

%% @spec (T::term()) -> string()
%% @doc Convert RFC 2254 parsed LHS or RHS string to an Erlang string.

rfc2254_string_ize({any, S}) ->
    rfc2254_unescape(S);
rfc2254_string_ize(star) ->
    star;
rfc2254_string_ize(L) when is_list(L) ->
    L2 = [rfc2254_string_ize(X) || X <- L],
    case lists:member(star, L2) of
        true ->
            L3 = lists:foldl(fun(star, Acc)                   -> [star|Acc];
                                (S,    [H|T]) when is_list(H) -> [(H++S)|T];
                                (S,    Acc)                   -> [S|Acc]
                             end, [], L2),
            {stars, lists:reverse(L3)};
        false ->
            %% Good heavens, this is icky.  Gotta unwrap 1 or 2 layers
            %% of lists & tuples, uff da.
            L3 = lists:flatten(L2),
            {string, lists:flatten(lists:map(fun({string, X}) -> X;
                                                (X)           -> X
                                             end, L3))}
    end.

rfc2254_unescape(S) ->
    rfc2254_unescape(S, []).

rfc2254_unescape([], Acc) ->
    lists:reverse(Acc);
rfc2254_unescape([$\\, A, B|T], Acc) ->
    rfc2254_unescape(T, [convert_hex([A, B])|Acc]);
rfc2254_unescape([H|T], Acc) ->
    rfc2254_unescape(T, [H|Acc]).

%% @spec (Digits::list()) -> integer()
%% @doc Convert a list of hex digits (in ASCII) to a positive integer.

convert_hex(Digits) ->
    convert_hex(Digits, 0).

convert_hex([], Sum) ->
    Sum;
convert_hex([D|Ds], Sum) ->
    DVal = ascii_hex_to_val(D),
    convert_hex(Ds, (Sum * 16) + DVal).

ascii_hex_to_val(D) when D >= $0, D =< $9 ->
    D - $0;
ascii_hex_to_val(D) when D >= $a, D =< $f ->
    10 + D - $a;
ascii_hex_to_val(D) when D >= $A, D =< $F ->
    10 + D - $A.

%% @spec (Term::term(), MaxSize::integer()) -> term()
%% @doc Reduce the size of a (possibly enormous) term to something smaller.
%% <tt>MaxSize</tt> is the maximum number of deep terms found
%% inside <tt>Term</tt>.
%%
%% Ideally, the <tt>%MaxLen</tt> atom would only appear at the place
%% where the length limit was exceeded ... but I don't care enough to
%% fix that bug/feature.

prune_term(Term, MaxSize) when is_list(Term); is_tuple(Term) ->
    {Acc2, _MaxSize2} = prune_term(Term, MaxSize, []),
    lists:flatten(Acc2);
prune_term(Term, _MaxSize) ->
    Term.

prune_term(_Term, MaxSize, Acc) when MaxSize =< 0 ->
    {['%MaxLen'|Acc], MaxSize};
prune_term(Term, MaxSize, Acc) when is_list(Term) ->
    {Acc2, MaxSize2} = prune_term_list(Term, MaxSize - 1, []),
    {['%StartList', Acc2, '%EndList'|Acc], MaxSize2};
prune_term(Term, MaxSize, Acc) when is_tuple(Term) ->
    {Acc2, MaxSize2} = prune_term_list(tuple_to_list(Term), MaxSize - 1, []),
    {['%StartTuple', Acc2, '%EndTuple'|Acc], MaxSize2};
prune_term(Term, MaxSize, Acc) when byte_size(Term) > 48 ->
    {SubBin, _} = split_binary(Term, 48),
    NewBin = list_to_binary([SubBin, "[...]"]),
    {[NewBin|Acc], MaxSize - 1};
prune_term(Term, MaxSize, Acc) ->               % Any other term type
    {[Term|Acc], MaxSize - 1}.

prune_term_list(_L, MaxSize, Acc) when MaxSize =< 0 ->
    {['%MaxLen'|Acc], MaxSize};
prune_term_list([], MaxSize, Acc) ->
    {Acc, MaxSize - 1};
prune_term_list([H|T], MaxSize, Acc) ->
    {Acc2, MaxSize2} = prune_term(H, MaxSize, []),
    prune_term_list(T, MaxSize2 - 1, [Acc|Acc2]).

%% @spec () -> ok
%% @doc Simple unit test for prune_term/2.

prune_term_test() ->
    F = fun(T, N) ->
                io:format("T   = ~p\n", [T]),
                io:format("Res = ~p\n", [prune_term(T, N)])
        end,
    F(1, 100),
    F(a, 100),
    F(self(), 100),
    F([1, 2, [a1, a2, a3], 3], 100),
    F([1, 2, [a1, a2, {b5, b6}, a3], 3], 100),
    F([1, 2, [a1, a2, <<"blarf">>, <<"blarfblarfblarfblarfblarfblarfblarfblarfblarfblarfblarfblarfblarfblarfblarfblarfblarfblarf">>, {b5, b6}, a3], 3], 100),
    F([1, 2, [a1, a2, {b5, b6, b7, b8}, a3], 3], 14),
    F({1, 2, [a1, a2, {b5, b6, b7, b8}, a3], 3}, 14),
    ok.

%% @spec (string()) -> list()
%% @doc Convert a list/binary string to a lower case list.

to_lower(Word) when is_list(Word) ->
    lists:map(fun(C) when $A =< C, C =< $Z ->
                      $a + (C - $A);
                 (C) ->
                      C
              end, Word);
to_lower(B) when is_binary(B) ->
    to_lower(list_ify(B)).

%% @spec (string()) -> list()
%% @doc Convert a list/binary string to a upper case list.

to_upper(Word) when is_list(Word) ->
    lists:map(fun(C) when $a =< C, C =< $z ->
                      $A + (C - $a);
                 (C) ->
                      C
              end, Word);
to_upper(B) when is_binary(B) ->
    to_upper(list_ify(B)).

%% @spec (list(), list() | char()) -> list()
%% @doc Join the elements of a list with a separator string or character,
%% like Perl's "join" but with its arguments in the reverse order.

join(L, Sep) ->
    lists:flatten(join2(L, Sep)).

join2([A, B|Rest], Sep) ->
    [A, Sep|join2([B|Rest], Sep)];
join2(L, _Sep) ->
    L.

%% @spec (binary(), integer()) -> list(binary())
%% @doc Split a binary into a list of sub-binaries, sortof like string:tokens/2
%%      but with only a single separator character.

split_bin_on_char(B, SplitChar) ->
    split_bin_on_char(B, SplitChar, 0, 0, size(B), []).

split_bin_on_char(B, _, Start, Len, BSize, Acc) when Start + Len =:= BSize ->
    <<_:Start/binary, Sub:Len/binary>> = B,
    lists:reverse([Sub|Acc]);
split_bin_on_char(B, SplitChar, Start, Len, BSize, Acc) ->
    <<_:Start/binary, Sub:Len/binary, C:8/integer, _/binary>> = B,
    if C =:= SplitChar ->
            split_bin_on_char(B, SplitChar, Start + Len + 1, 0, BSize,
                              [Sub|Acc]);
       true ->
            split_bin_on_char(B, SplitChar, Start, Len+1, BSize, Acc)
    end.

%% @spec (binary()) -> binary()
%% @doc Chop off the trailing CR-LF or only LF from a binary line.

chomp_bin(Line) ->
    Len = size(Line),
    case {Len, Line} of
        {0, _}                    -> <<>>;
        {1, <<"\n">>}             -> <<>>;
        {2, <<?CRLF>>}            -> <<>>;
        {2, <<C:1/binary, "\n">>} -> C;
        _ ->
            Len1 = Len - 1,
            Len2 = Len - 2,
            case Line of
                %% Intentionally only match 2 cases!
                <<Line2:Len2/binary, ?CRLF>>  -> Line2;
                <<Line2:Len1/binary, "\n">>   -> Line2
            end
    end.


%% @spec () -> list()
%% @doc Get the current list of set alarms: <tt>{Name, Description}</tt>

get_alarms() ->
    alarm_handler:get_alarms().

%% @spec (term(), term()) -> ok
%% @doc Set an alarm by <tt>Name</tt> and description term.
%%
%% <tt>Fun</tt> must be arity 0.

set_alarm(Name, Descr) ->
    set_alarm(Name, Descr, fun() -> ok end).

%% @spec (term(), term(), fun()) -> ok
%% @doc Set an alarm by <tt>Name</tt> and description term.
%%
%% <tt>Fun</tt> must be arity 0.

set_alarm(Name, Descr, Fun) ->
    set_alarm(Name, Descr, Fun, get_alarms()).

%% @spec (term(), term(), fun(), list()) -> ok
%% @doc Set an alarm by <tt>Name</tt>, description term, and list of
%% alarms currently set.
%%
%% <tt>Fun</tt> must be arity 0.

set_alarm(Name, Descr, Fun, CurrentList) ->
    case alarm_set_p(Name, CurrentList) of
        true -> ok;
        false ->
            alarm_handler:set_alarm({Name, Descr}),
            Fun(),
            ok
    end.

%% @spec (term()) -> ok
%% @doc Clear an alarm by <tt>Name</tt>.

clear_alarm(Name) ->
    clear_alarm(Name, fun() -> ok end).

%% @spec (term(), fun()) -> ok
%% @doc Clear an alarm by <tt>Name</tt>.
%%
%% <tt>Fun</tt> must be arity 0.

clear_alarm(Name, Fun) ->
    clear_alarm(Name, Fun, get_alarms()).

%% @spec (term(), fun(), list()) -> ok
%% @doc Clear an alarm by <tt>Name</tt> and a list of alarms
%% current set.
%%
%% <tt>Fun</tt> must be arity 0.

clear_alarm(Name, Fun, CurrentList) ->
    case alarm_set_p(Name, CurrentList) of
        true ->
            alarm_handler:clear_alarm(Name),
            Fun(),
            ok;
        false ->
            ok
    end.

%% @spec (term()) -> true | false
%% @doc Check if alarm <tt>Name</tt> is set.

alarm_set_p(Name) ->
    alarm_set_p(Name, get_alarms()).

%% @spec (term(), list()) -> true | false
%% @doc Check if alarm <tt>Name</tt> is set, give a list of alarms.

alarm_set_p(Name, AlarmList) ->
    lists:keymember(Name, 1, AlarmList).

%% @spec (server_spec()) -> {ok, monitor_ref()} | error
%% @doc Simplify the arcane art of <tt>erlang:monitor/1</tt>:
%%      create a monitor.
%%
%% The arg may be a PID or a {registered_name, node} tuple.
%% In the case of the tuple, we will use rpc:call/4 to find the
%% server's actual PID before calling erlang:monitor();
%% therefore there is a risk of blocking by the RPC call.
%% To avoid the risk of blocking in this case, use make_monitor/2.

make_monitor(Pid) when is_pid(Pid) ->
    make_monitor2(Pid);
make_monitor({Name, Node}) ->
    case catch rpc:call(Node, erlang, whereis, [Name]) of
        Pid when is_pid(Pid) ->
            make_monitor2(Pid);
        _ ->
            error
    end.

make_monitor(Name, Node) ->
    make_monitor2({Name, Node}).                % use as-is

make_monitor2(Spec) ->                          % Private func
    case catch erlang:monitor(process, Spec) of
        MRef when is_reference(MRef) ->
            receive
                {'DOWN', MRef, _, _, _} ->
                    error
            after 0 ->
                    {ok, MRef}
            end;
        _ ->
            error
    end.

%% @spec (pid()) -> {ok, monitor_ref()} | error
%% @doc Simplify the arcane art of <tt>erlang:demonitor/1</tt>:
%%      destroy a monitor.

unmake_monitor(MRef) ->
    erlang:demonitor(MRef),
    receive
        {'DOWN', MRef, _, _, _} ->
            ok
    after 0 ->
            ok
    end.

%% @spec () -> integer | throw
%% @doc Return total amount of memory used by all runtime allocators
%% when the emulator is run with the <tt>+Mis true</tt> flag.

total_memory() ->
    [{total, [{sizes, T, _, _}, _]}] = instrument:memory_status(total),
    T.

%% @spec () -> integer | throw
%% @doc Return total amount of megabytes of memory used by all runtime
%% allocators %% when the emulator is run with the <tt>+Mis true</tt>
%% flag.

total_megs() ->
    total_memory() / (1024*1024).

%% @spec (fun(), term(), list(), fun(), integer) -> term()
%% @doc Like lists:foldl(), but takes another func, the outer function,
%%      and applies the outer function to the accumulator every Count
%%      iterations.
%% The outer function is also called after the list has been
%% exhausted.  The outer function is permitted to mutate the
%% accumulator.
%%
%% Below is an implementation of `gmt_util:list_chop/2' using this function.
%% It is approximately 30-50% slower than `list_chop/2'.
%% `eqc:quickcheck(eqc:numtests(5000, gmt_util_eqc:prop_list_chop()))' says
%% that these two functions are equivalent.
%%
%% ```
%% list_chop2([], MaxLen) ->
%%     [];
%% list_chop2(L, MaxLen) ->
%%     {_, RevL} = list_chunkfoldl(
%%                %% Inner fun: add X to the small half of the accumulator.
%%                fun(X, {Small, Big}) -> {[X|Small], Big} end,
%%                {[], []}, L,
%%                %% Outer fun: add small list to the big half of the
%%                %%            accumulator, reset the small list to [].
%%                fun({Small, Big}) -> {[], [lists:reverse(Small)|Big]} end,
%%                MaxLen),
%%     lists:reverse(RevL).
%% '''

list_chunkfoldl(F_inner, Acc, L, F_outer, Count) ->
    list_chunkfoldl(Count, F_inner, Acc, L, F_outer, Count).

list_chunkfoldl(0, _F_inner, Acc, [], F_outer, _Count) ->
    %% This clause is here to avoid calling the outer func twice in rare cases.
    F_outer(Acc);
list_chunkfoldl(0, F_inner, Acc, L, F_outer, Count) ->
    list_chunkfoldl(Count, F_inner, F_outer(Acc), L, F_outer, Count);
list_chunkfoldl(_, _F_inner, Acc, [], F_outer, _Count) ->
    F_outer(Acc);
list_chunkfoldl(C, F_inner, Acc, [H|T], F_outer, Count) ->
    list_chunkfoldl(C - 1, F_inner, F_inner(H, Acc), T, F_outer, Count).


%% @spec (list(), integer()) -> list(list())
%% @doc Chop a single list into a list of lists where each sublist contains
%%      a maximum of <tt>MaxMembers</tt>.

%% NOTE: I feel like I've re-invented this function yet again, but I can't
%% remember the details: when, its name, ... {sigh}

list_chop(L, MaxLen) when is_list(L), is_integer(MaxLen), MaxLen > 0 ->
    list_chop(L, [], 0, MaxLen).

list_chop([_|_] = L, Acc, AccLen, MaxLen) when AccLen == MaxLen ->
    [lists:reverse(Acc)|list_chop(L, MaxLen)];
list_chop([H|T], Acc, AccLen, MaxLen) ->
    list_chop(T, [H|Acc], AccLen + 1, MaxLen);
list_chop([], [], _, _) ->
    [];                                         % Avoid trailing empty list
list_chop([], Acc, _, _) ->
    [lists:reverse(Acc)].

%% @spec (integer(), list(tuple())) -> list({term(), list()})
%% @doc Partition a list based on tuple position Pos.
%%
%% The list must be sorted by tuple element Pos before calling,
%% e.g. by lists:keysort/2.

list_keypartition(Pos, [_|_] = TupleList) ->
    list_kp(element(Pos, hd(TupleList)), TupleList, Pos, []);
list_keypartition(_Pos, []) ->
    [].

list_kp(Key, [H|T], Pos, Acc) when element(Pos, H) == Key ->
    list_kp(Key, T, Pos, [H|Acc]);
list_kp(Key, [_|_] = TupleList, Pos, Acc) ->
    [{Key, lists:reverse(Acc)}|list_keypartition(Pos, TupleList)];
list_kp(Key, [], _Pos, Acc) ->
    [{Key, lists:reverse(Acc)}].


%% @spec (list()) -> list(list())
%% @doc Calculate all permutations of an ordered list (assuming no duplicates).

permutations([]) ->[[]];
permutations(L) ->
    [[H|T] || H <- L, T <- permutations(L--[H])].

%% @spec (list()) -> list(list())
%% @doc Calculate all combinations of an ordered list (assuming no duplicates).

combinations([]) ->[[]];
combinations(L) ->
    ordsets:from_list(
      permutations(L) ++ [P || H <- L, P <- combinations(L--[H])]).

%% @spec (string()) -> term()
%% @doc Parse a single Erlang expression in a string and return the term
%% that the string represents.
%%
%% If a trailing "." is omitted from the string, one is appended before
%% tokenizing.

parse_erlang_single_expr(Str0) ->
    Str = case lists:last(Str0) of
              $. -> Str0;
              _  -> Str0 ++ "."
          end,
    {ok, Ts, _} = erl_scan:string(Str),
    {ok, Rs} = erl_parse:parse_exprs(Ts),
    {value, Val, _} = erl_eval:exprs(Rs, []),
    Val.

%% @spec (atom()) -> string()
%% @doc Find a module's base directory

find_base_directory(Module) ->
    begin
        {ok, P} = erl_prim_loader:get_path(),
        ModuleFile = atom_to_list(Module) ++ extension(),
        Pd = (catch lists:foldl
                      (fun(X,Acc) ->
                               M = filename:join([X, ModuleFile]),
                               %% The file server probably not started either,
                               %% has to use raw interface.
                               case file:raw_read_file_info(M) of
                                   {ok,_} ->
                                       %% Found our own module in the path,
                                       %% lets bail out with the base of this
                                       %% directory
                                       Y = filename:split(X),
                                       throw(filename:join
                                               (lists:sublist
                                                  (Y,length(Y) - 1)));
                                   _ ->
                                       Acc
                               end
                       end,
                       false,P)),
        case Pd of
            false ->
                exit(base_directory_indeterminate);
            _ ->
                Pd
        end
    end.

extension() ->
    %% erlang:info(machine) returns machine name as text in all uppercase
    "." ++ [X + $a - $A || X <- erlang:system_info(machine)].


wait_for_processes(Procs) when is_list(Procs) ->
    do_wait_for_processes(Procs, infinity);
wait_for_processes(Procs) when is_atom(Procs) ->
    do_wait_for_processes([Procs], infinity);
wait_for_processes(Procs) ->
    {error, {badarg, Procs}}.

%% @spec (list:atoms() | atom, Timeout) -> ok | timeout | {error, Reason}
%% @doc Wait one time for each process to be registered and active.

wait_for_processes(Procs, Timeout) when is_list(Procs), Timeout == infinity ->
    do_wait_for_processes(Procs, Timeout);
wait_for_processes(Procs, Timeout) when is_list(Procs), is_integer(Timeout), Timeout >= 0 ->
    do_wait_for_processes(Procs, Timeout);
wait_for_processes(Procs, Timeout) when is_atom(Procs), Timeout == infinity ->
    do_wait_for_processes([Procs], Timeout);
wait_for_processes(Procs, Timeout) when is_atom(Procs), is_integer(Timeout), Timeout >= 0 ->
    do_wait_for_processes([Procs], Timeout);
wait_for_processes(Procs, Timeout) ->
    {error, {badarg, Procs, Timeout}}.

do_wait_for_processes(_Procs, 0) ->
    timeout;
do_wait_for_processes(Procs, Timeout) ->
    Self = self(),
    Pid = spawn_link(?MODULE, wait_for_processes_loop, [Self, Procs]),
    receive
        {Self, Pid, Res} ->
            Res;
        {'EXIT', Pid, _} ->
            timeout
    after Timeout ->
            unlink(Pid),
            exit(Pid, timeout),
            timeout
    end.

wait_for_processes_loop(From, Procs) ->
    process_flag(trap_exit, true),
    Res = wait_for_loop(Procs),
    From ! {From, self(), Res},
    exit(normal).

wait_for_loop([H|T]=L) ->
    case whereis(H) of
        undefined ->
            timer:sleep(500),
            wait_for_loop(L);
        Else ->
            case process_info(Else, status) of
                {status, _Status} ->
                    wait_for_loop(T);
                _ ->
                    timer:sleep(500),
                    wait_for_loop(L)
            end
    end;
wait_for_loop([]) ->
    ok.

io_list_len(B) when is_binary(B) ->
    byte_size(B);
io_list_len(L) when is_list(L) ->
    lists:foldl(fun(X, Len) -> Len + io_list_len(X) end, 0, L);
io_list_len(C) when C >= 0; C =< 255 ->
    1.

%% @spec (list(term())) -> list(term())
%% @doc Remove all duplicate terms from a pre-sorted list.
list_unique([X,X|Xs]) -> list_unique([X|Xs]);
list_unique([X|Xs])   -> [X|list_unique(Xs)];
list_unique([])       -> [].

%% @spec (list(term())) -> list(term())
%% @doc Remove <em>all</em> duplicate terms from an <em>unsorted</em> list.
list_unique_u([X|Xs]) ->
    F = fun(Y) when Y == X -> false;
           (_)             -> true
        end,
    [X|list_unique_u(lists:filter(F, Xs))];
list_unique_u([]) ->
    [].

%% @spec (list({atom(), term()})) -> list({atom(), term()})
%% @doc Remove all duplicate key from a pre-sorted proplist.
%%      Value from later occurence will be used.
proplist_unique([{P,_},{P,V}|Ps]) -> proplist_unique([{P,V}|Ps]);
proplist_unique([P|Ps])           -> [P|proplist_unique(Ps)];
proplist_unique([])               -> [].

%% @spec (proplist(), proplist()) -> proplist()
%% @doc Update OldPropList value with NewPropList.  If new key is found
%%      in NewPropList, add property to OldPropList.
%% Note: side effect: keys are sorted and the duplicated keys are removed
%% > C = [{a, 15}, {c, 20}, {bb, 30}].
%% > D = [{e}, {d, 40}].
%% > proplist_update(C, D).
%% [{a,15},{bb,30},{c,20},{d,40},{e}]
%%
%% > C2 = [{a, 15}, {c, 21}, {bb, 31}].
%% > proplist_update(C, C2).
%% [{a,15},{bb,31},{c,21}]
proplist_update(OldPropList, NewPropList) ->
    proplist_unique(lists:keysort(1, OldPropList ++ NewPropList)).

%% @spec keyfindIgnoreCase(Key, N, TupleList) -> Tuple | false
%% @doc Similar to lists:keyfind except it ignore case when inputs are string().
%%      I wonder somewhere BIF must have implemented it...
keyfindIgnoreCase(Key, N, TupleList) when not is_list(Key) ->
    lists:keyfind(Key, N, TupleList);
keyfindIgnoreCase(Key, N, TupleList) when is_list(TupleList), is_list(Key) ->
    keyfindIgnoreCase_int(string:to_lower(Key), N, TupleList).

keyfindIgnoreCase_int(KeyLower, N, TupleList) ->
    case TupleList of
        [] ->
            false;
        [H | T] when tuple_size(H) >= N, is_list(element(N, H)) ->
            case string:to_lower(element(N, H)) of
                KeyLower ->
                    H;
                _ ->
                    keyfindIgnoreCase_int(KeyLower, N, T)
            end;
        [_ | T] ->
            keyfindIgnoreCase_int(KeyLower, N, T)
    end.

%% @spec (string()) -> integer()
%% @doc Handy function for configuration to express size units in
%%      a more readable form and return number of bytes.
%%      Examples: 12->12, 12K->12288, 12M->12582912, 12G->12884901888
list_to_bytes(Size) when is_list(Size) ->
    Rev = lists:reverse(Size),
    {Multiplier, Size2} = case hd(Rev) of
                              $K ->
                                  {1024, lists:reverse(tl(Rev))};
                              $M ->
                                  {1024*1024, lists:reverse(tl(Rev))};
                              $G ->
                                  {1024*1024*1024, lists:reverse(tl(Rev))};
                              $T ->
                                  {1024*1024*1024*1024, lists:reverse(tl(Rev))};
                              $P ->
                                  {1024*1024*1024*1024*1024, lists:reverse(tl(Rev))};
                              _ ->
                                  {1, Size}
                          end,
    Size3 = case catch list_to_integer(Size2) of
                {'EXIT', _Badarg} ->
                    list_to_float(Size2);
                Int ->
                    Int
            end,
    %% use round in case of float to allow stuff like 5.5M
    round(Multiplier * Size3).

%% @spec () -> {term(),node()}
%% @doc construct a global reference
global_reference() ->
    {erlang:make_ref(),erlang:node()}.

%% @spec (Ref::term()) -> {term(),node()}
%% @doc construct a global reference
global_reference(Ref) ->
    {Ref,erlang:node()}.

%% @spec (Ref2::term(), Ref1::term()) -> 0 | -1 | 1
%% @doc compare two references
ref_diff(Ref2, Ref1) ->
    if Ref2 =:= Ref1 ->
            0;
       Ref2 < Ref1 ->
            -1;
       true ->
            1
    end.

%% @spec ({Ref2::term(), Node2::node()}, {Ref1::term(), Node1::node()}) -> 0 | -1 | 1
%% @doc compare two global references
global_reference_cmp({Ref2, Node2}, {Ref1, Node1}) ->
    Diff = ref_diff(Ref2, Ref1),
    if Diff =/= 0 ->
            Diff;
       true ->
            if Node2 =:= Node1 ->
                    0;
               Node2 < Node1 ->
                    -1;
               true ->
                    1
            end
    end.

%% @spec (Gr2::term(), Gr1::term()) -> true | false
%% @doc less two global references
global_reference_less(Gr2, Gr1) ->
    global_reference_cmp(Gr2, Gr1) =:= -1.

%% @spec () -> binary()
%% @doc construct a global reference and encode as binary with no
%% whitespaces
global_reference_binary() ->
    global_reference_binary(global_reference()).

%% @spec (Gr::term()) -> binary()
%% @doc encode a global reference as binary with no whitespaces
global_reference_binary(Gr) ->
    base64:encode(erlang:term_to_binary(Gr)).

%% @spec (list()) -> list()
%% @doc randomly sort a list
randomize_list(L) ->
    randomize_list(L, 1000000).

%% @spec (list(), integer()) -> list()
%% @doc randomly sort a list
randomize_list(L, N) ->
    WrapL = [{random:uniform(N), X} || X <- L],
    TempL = lists:sort(WrapL),
    [X || {_, X} <- TempL].

-type plist() :: list(atom() | {atom(), term()}).
-spec proplists_int_copy(plist(), plist(), list(atom())) -> plist().
proplists_int_copy(Dest, _Src, []) ->
    Dest;
proplists_int_copy(Dest, Src, [Key|NextKeys]) ->
    case proplists:get_value(Key, Src) of
        Val when is_integer(Val) ->
            proplists_int_copy([{Key, Val}|Dest], Src, NextKeys);
        _ ->
            proplists_int_copy(Dest, Src, NextKeys)
    end.

%%%
%%% Misc edoc stuff
%%%

%% @type cal_date() = {integer(), integer(), integer()}
%% @type cal_time() = {integer(), integer(), integer()}
%% @type calendar_date() = {cal_date(), cal_time()}
%% @type enum_list() = [enum_things()]
%% @type enum_things() = integer() | {string(), integer()}
%% @type generalized_time() = string() | binary()
%% @type kv_list() = list({atom(), string()})
%% @type monitor_ref() = term(). An opaque term used for Erlang monitors.
%% @type msisdn_intern() = integer()
%% @type stringish() = string() | binary()

%% for debugging %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dbgon(Module) ->
    case dbg:tracer() of
        {ok,_} ->
            dbg:p(all,call),
            {ok, _} = dbg:tpl(Module, [{'_',[],[{return_trace}, {exception_trace}]}]),
            ok;
        Else ->
            Else
    end.

dbgon(Module, Fun) when is_atom(Fun) ->
    {ok,_} = dbg:tracer(),
    dbg:p(all,call),
    {ok, _} = dbg:tpl(Module, Fun, [{'_',[],[{return_trace}, {exception_trace}]}]),
    ok;

dbgon(Module, File) when is_list(File) ->
    {ok,_} = dbg:tracer(port, dbg:trace_port(file, File)),
    dbg:p(all,call),
    {ok, _} = dbg:tpl(Module, [{'_',[],[{return_trace}, {exception_trace}]}]),
    ok;

dbgon(Module, Port) when is_integer(Port) ->
    {ok,_} = dbg:tracer(port, dbg:trace_port(ip, Port)),
    dbg:p(all,call),
    {ok, _} = dbg:tpl(Module, [{'_',[],[{return_trace}, {exception_trace}]}]),
    ok.

dbgadd(Module) ->
    {ok, _} = dbg:tpl(Module, [{'_',[],[{return_trace}, {exception_trace}]}]),
    ok.

dbgadd(Module, Fun) ->
    {ok, _} = dbg:tpl(Module, Fun, [{'_',[],[{return_trace}, {exception_trace}]}]),
    ok.

dbgdel(Module) ->
    {ok, _} = dbg:ctpl(Module),
    ok.

dbgdel(Module, Fun) ->
    {ok, _} = dbg:ctpl(Module, Fun),
    ok.

dbgoff() ->
    dbg:stop().

node_left() ->
    node_left(node()).
node_left(Node) ->
    Snode = list_ify(Node),
    case catch lists:split(string:chr(Snode,$@) - 1, Snode) of
        {Left, _Right} ->
            Left;
        _ ->
            badarg
    end.

node_right() ->
    node_right(node()).
node_right(Node) ->
    Snode = list_ify(Node),
    case catch lists:split(string:chr(Snode,$@), Snode) of
        {_Left, Right} ->
            Right;
        _ ->
            badarg
    end.
