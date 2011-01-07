%%%----------------------------------------------------------------------
%%% Copyright (c) 2007-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_edoc.erl
%%% Purpose : erlang helpers
%%%----------------------------------------------------------------------

%%%
%%% NOTE: A minimal set of "erlang" helpers.
%%%

-module(gmt_erlang).

-compile(export_all).

%% @spec term_bytes(term()) -> integer()
%% @doc estimate the effective bytes of a term in words
term_bytes(Term) when is_binary(Term) ->
    byte_size(Term);
term_bytes(Term) ->
    byte_size(term_to_binary(Term)).

%% @doc Calls Fun(Elem, AccIn) on successive elements A of Term,
%% starting with AccIn == Acc0. Fun/2 must return a new accumulator
%% which is passed to the next call. The function returns the final
%% value of the accumulator. Acc0 is returned if the term is empty.
term_foldl(F, Accu, [Hd|Tail]) ->
    term_foldl(F, F(Hd, Accu), Tail);
term_foldl(F, Accu, []) when is_function(F, 2) ->
    Accu;
term_foldl(F, Accu, T) when is_tuple(T) ->
    term_foldl(F, Accu, tuple_to_list(T));
term_foldl(F, Accu, T) ->
    F(T, Accu).

%% @spec binary_to_integer(Bin::binary()) -> integer()
%% @doc convert binary to integer
binary_to_integer(Bin) when is_binary(Bin) ->
    list_to_integer(binary_to_list(Bin)).

%% @spec integer_to_binary(Int::integer()) -> binary()
%% @doc convert integer to binary
integer_to_binary(Int) when is_integer(Int) ->
    list_to_binary(integer_to_list(Int)).

%% @spec integer_to_binary(Int::integer(), MinDigits::integer()) -> binary()
%% @doc convert integer to binary with a minimum number of zero-padded digits
integer_to_binary(Int, MinDigits) when is_integer(Int) ->
    list_to_binary(string:right(integer_to_list(Int), MinDigits, $0)).

%% @spec binary_to_pid(Bin::binary()) -> pid()
%% @doc convert binary to pid
binary_to_pid(Bin) when is_binary(Bin) ->
    list_to_pid(binary_to_list(Bin)).

%% @spec pid_to_binary(Pid::pid()) -> binary()
%% @doc convert pid to binary
pid_to_binary(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid)).

%% @spec binary_to_node_n_pid(Bin::binary()) -> {node(), pid()}
%% @doc convert binary to node and pid
binary_to_node_n_pid(Bin) when is_binary(Bin) ->
    case binary_to_term(Bin) of
        {Node, Pid}=Term when is_atom(Node) andalso is_pid(Pid) ->
            Term
    end.

%% @spec node_n_pid_to_binary({Node::node(), Pid::pid()}) -> binary()
%% @doc convert node and pid to binary
node_n_pid_to_binary({Node, Pid}=Term) when is_atom(Node), is_pid(Pid) ->
    term_to_binary(Term).

%% @spec is_char(X::term()) -> bool()
%% @doc Check if X is a printable character
is_char(X) ->
    if X < 0 -> false;
       X > 255 -> false;
       true -> true
    end.

%% @spec is_lowercase_alpha(X::term()) -> bool()
%% @doc Check if X is an lowercase alpha
is_lowercase_alpha(X) ->
    if X < $a -> false;
       X > $z -> false;
       true -> true
    end.

%% @spec is_uppercase_alpha(X::term()) -> bool()
%% @doc Check if X is an uppercase alpha
is_uppercase_alpha(X) ->
    if X < $A -> false;
       X > $Z -> false;
       true -> true
    end.

%% @spec is_alpha(X::term()) -> bool()
%% @doc Check if X is an alpha
is_alpha(X) ->
    is_lowercase_alpha(X) orelse is_uppercase_alpha(X).

%% @spec is_num(X::term()) -> bool()
%% @doc Check if X is a num
is_num(X) ->
    if X < $0 -> false;
       X > $9 -> false;
       true -> true
    end.

%% @spec is_alphanum(X::term()) -> bool()
%% @doc Check if X is a alphanum
is_alphanum(X) ->
    is_alpha(X) orelse is_num(X).

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

%% @spec bin_ify(X::term()) -> binary()
%% @doc Convert a term to a binary (if convertable).
bin_ify(X) when is_binary(X) ->
    X;
bin_ify(X) when is_list(X) ->
    list_to_binary(X);
bin_ify(X) when is_integer(X) ->
    list_to_binary(integer_to_list(X));
bin_ify(X) when is_atom(X) ->
    %% QQQ This appears to be faster than
    %% list_to_binary(atom_to_list()) every time.  Double-check using
    %% cprof and perhaps eprof.
    case get(X) of
        undefined -> B = list_to_binary(atom_to_list(X)),
                     put(X, B),
                     B;
        B -> B
    end.

%% @spec int_ify(X::term()) -> integer()
%% @doc Convert a term to an integer (if convertable).
int_ify(X) when is_integer(X) ->
    X;
int_ify(X) when is_binary(X) ->
    list_to_integer(binary_to_list(X));
int_ify(X) when is_list(X) ->
    list_to_integer(X).

%% @spec (integer() | string() | binary()) -> integer() | infinity
%% @doc Given a term, return infinity atom or an integer if
%% convertible
int_infinity_ify(infinity) ->
    infinity;
int_infinity_ify("infinity") ->
    infinity;
int_infinity_ify(<<"infinity">>) ->
    infinity;
int_infinity_ify(X) ->
    int_ify(X).

%% @spec timeout_ify(X::term()) -> integer() | infinity
%% @doc Convert a term to an integer or the atom 'infinity' (if
%% convertable).
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
    tuple_to_list(X).

%% @spec (L::list(), T::atom()) -> list()
%% @doc Return L if all members of L are of PSS type T or are
%% convertable to type T, else throw error.
%%
%% If conversion to type T is possible, conversion will be done, so
%% the return value may not be exactly equal to input argument L.
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

%% @spec (string()) -> term()
%% @doc Parse a single Erlang expression in a string and return the
%% term that the string represents.
%%
%% If a trailing "." is omitted from the string, one is appended
%% before tokenizing.
parse_erlang_single_expr(Str0) ->
    Str = case lists:last(Str0) of $. -> Str0; _ -> Str0 ++ "." end,
{ok, Ts, _} = erl_scan:string(Str),
{ok, Rs} = erl_parse:parse_exprs(Ts),
{value, Val, _} = erl_eval:exprs(Rs, []),
Val.

%% @spec (list(any())) -> any()
%% @doc 'apply' on a string or list of strings.
apply([Mod|[Fun|Args]]) ->
    case is_string(Args) of
        true ->
            erlang:apply(atom_ify(Mod), atom_ify(Fun), parse_erlang_single_expr(Args));
        false ->
            erlang:apply(atom_ify(Mod), atom_ify(Fun), [ parse_erlang_single_expr(list_ify(X)) || X <- Args ])
    end.
