%%%-------------------------------------------------------------------
%%% Copyright (C) 2013-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
%%% Copyright (c) 2008-2012 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : qc_gen.erl
%%% Purpose : QC generators
%%%-------------------------------------------------------------------

-module(qc_gen).

-ifdef(QC).

-compile(export_all).

-include("qc_impl.hrl").

%% helper for sizing down recursive generators
-define(SIZEDOWN(G),
        ?SIZED(Size, resize(round(math:sqrt(Size)),G))).


%%%-------------------------------------------------------------------
%%% Base Generators
%%%

%%%%%%
%% largeint/0
-ifdef(TRIQ).
largeint() ->
    int().
-endif.

%%%%%%
%% ulist
ulist(G) ->
    ?LET(Xs, list(G), Xs -- (Xs -- lists:usort(Xs))).


%%%%%%
%% atom
qc_atom() ->
    qc_atom([]).

qc_atom(Attrs) when is_list(Attrs) ->
    X = lists:usort(Attrs),
    StringAttrs = [ Attr || Attr <- X, Attr =/= nonundefined ],
    oneof([ok,true,false,?LET(L,qc_string(StringAttrs),list_to_atom(L))]
          ++ if StringAttrs =:= X -> [undefined]; true -> [] end).


%%%%%%
%% binary
qc_binary() ->
    qc_binary([]).

qc_binary(Attrs) when is_list(Attrs) ->
    ?LET(L,qc_string(Attrs),list_to_binary(L)).


%%%%%%
%% byte
qc_byte() ->
    choose(0, 255).


%%%%%%
%% proplist
qc_proplist() ->
    qc_proplist([]).

qc_proplist(Attrs) when is_list(Attrs) ->
    Key = qc_atom(),
    Val = qc_any(),
    G = {Key, Val},
    qc_list(G, Attrs).


%%%%%%
%% list
qc_list() ->
    G = qc_any(),
    qc_list(G).

qc_list(G) ->
    qc_list(G, []).

qc_list(G, Attrs) when is_list(Attrs) ->
    case lists:usort(Attrs) of
        [] ->
            list(G);
        [nonempty] ->
            non_empty(list(G))
    end.

%%%%%%
%% string
qc_string() ->
    qc_string([]).

qc_string(Attrs) when is_list(Attrs) ->
    case lists:usort(Attrs) of
        [] ->
            oneof([list(qc_byte())
                   ,  ?LET(Attrs1,oneof([[nonempty], [ascii], [ascii,nonempty], [asciiprintable], [asciiprintable,nonempty], [ascii_alphanumeric], [ascii_alphanumeric, nonempty]])
                           , qc_string(Attrs1))
                  ]);
        [nonempty] ->
            qc_list(qc_byte(),[nonempty]);
        [CharAttr] ->
            list(qc_char([CharAttr]));
        [CharAttr, nonempty] when is_atom(CharAttr) ->
            qc_list(qc_char([CharAttr]),[nonempty])
    end.


%%%%%%
%% term
qc_term() ->
    qc_term([]).

qc_term(Attrs) ->
    qc_any(Attrs).


%%%%%%
%% tuple
qc_tuple() ->
    qc_tuple([]).

qc_tuple(Attrs) when is_list(Attrs) ->
    G = qc_any(),
    ?LET(L,qc_list(G, Attrs),
         list_to_tuple(L)).


%%%-------------------------------------------------------------------
%%% Other Generators
%%%


%%%%%
%% any
qc_any() ->
    qc_any([]).

qc_any(Attrs) ->
    %% ISSUE: This will fall into an infinite loop, because of incompatibilities
    %%        in size distribution.
    %% BONUS: Can also try the predefined any() type.
    ?SIZEDOWN(
       case X = lists:usort(Attrs) of
           [] ->
               %% @todo must be a better definition!!!
               oneof([
                      qc_atom(), qc_string(), qc_binary(), qc_tuple(), qc_list(), qc_proplist(), qc_term()
                      ,  ?LET(Attrs1,oneof([[nonempty], [nonundefined], [nonempty,nonundefined]])
                              , qc_any(Attrs1))
                     ]);
           [nonempty] ->
               oneof([qc_atom(X), qc_string(X), qc_binary(X), qc_tuple(X), qc_list(X), qc_proplist(X), qc_term(X)]);
           [nonundefined] ->
               oneof([qc_atom(X), qc_string(), qc_binary(), qc_tuple(), qc_list(), qc_proplist(), qc_term(X)]);
           [nonempty,nonundefined] ->
               Y = [nonempty],
               oneof([qc_atom(X), qc_string(Y), qc_binary(Y), qc_tuple(Y), qc_list(Y), qc_proplist(Y), qc_term(X)])
       end).


%%%%%%
%% char
qc_char(Attrs) when is_list(Attrs) ->
    case lists:usort(Attrs) of
        [ascii] ->
            choose(0,127);
        [asciiprintable] ->
            choose(32,126);
        [ascii_alphanumeric] ->
            oneof([
                   choose($A, $Z),
                   choose($a, $z),
                   choose($0, $9)
                  ])
    end.


%%%%%%
%% timeout - avoid a real timeout by starting from 100 sec
qc_timeout() ->
    oneof([infinity,choose(100000,10000000)]).

qc_timeout(Attrs) when is_list(Attrs) ->
    case lists:usort(Attrs) of
        [noninfinite] ->
            choose(100000,10000000);
        [] ->
            qc_timeout()
    end.


%%%%%%
%% Bool
%%
%% syntactic sugar for bool(), true/false value could be set thru
%% proplist Attrs (Also a better way for qc_choose if there are only 2
%% items).
%%
%% For examples:
%% Attrs = [{true, 1}, {false, 0}].
%% Attrs = [{true, "1"}, {false, "0"}]
%% Attrs = [{true, "true"}, {false, "false"}]
%% Attrs = [{true, "dog"}, {false, "cat"}]
%% Attrs = [{true, ok}, {false, ng}]
qc_bool(Attrs) ->
    ?LET(Bool, bool(), proplists:get_value(Bool, Attrs, Bool)).

qc_bool(True, False) ->
    qc_bool([{true, True}, {false, False}]).


%%%%%%
%% integer
%% TODO(gki): make attr: [length, min, max, base]
qc_int_list(Length, Base) ->
    Range = round(math:pow(Base, Length)) - 1, % int() is too small
    %% BONUS: Can also try the predefined integer() type.
    ?SUCHTHAT(L,
              ?LET(I, choose(0, Range), string:right(erlang:integer_to_list(abs(I), Base), Length, $0)),
              length(L) =:= Length).


%%%%%%
%% varchar
%% varchar() -> random size nonempty ascii aphanumeric string
%% varchar(S) -> fixed size nonempty ascii aphanumeric string
%% varchar(Min, Max) -> ranged nonempty ascii aphanumeric string
qc_varchar() ->
    qc_string([ascii_alphanumeric, nonempty]).

qc_varchar(0) ->
    [];
qc_varchar(S) when is_integer(S) ->
    qc_varchar(S, S).

qc_varchar(Min, Max) when is_integer(Min) andalso is_integer(Max) ->
    qc_varchar(Min, Max, []).

qc_varchar(_, Max, Buffer) when length(Buffer) > Max ->
    string:substr(Buffer, 1, Max);
qc_varchar(Min, Max, Buffer) when length(Buffer) < Min ->
    ?LET(S, qc_varchar(), qc_varchar(Min, Max, Buffer ++ S));
qc_varchar(_, _, Buffer) ->
    Buffer.


%%%%%%
%% time
qc_date_yymmdd() ->
    ?LET({Y,M}, {choose(0, 99), choose(1, 12)}, qc_date_yymmdd(Y, M)).

qc_date_yymmdd(Y, M) when is_integer(Y) andalso is_integer(M) ->
    NumDays = calendar:last_day_of_the_month(Y, M),
    ?LET(D, choose(1, NumDays),
         string:right(integer_to_list(Y), 2, $0) ++
             string:right(integer_to_list(M), 2, $0) ++
             string:right(integer_to_list(D), 2, $0)
        ).


qc_time_hhmmss() ->
    ?LET({H,M,S}, {choose(0, 23), choose(0, 59), choose(0, 59)},
         string:right(integer_to_list(H), 2, $0) ++
             string:right(integer_to_list(M), 2, $0) ++
             string:right(integer_to_list(S), 2, $0)).


qc_time_hhmmssms() ->
    ?LET({H,M,S,Ms}, {choose(0, 23), choose(0, 59), choose(0, 59), choose(0, 999)},
         string:right(integer_to_list(H), 2, $0) ++
             string:right(integer_to_list(M), 2, $0) ++
             string:right(integer_to_list(S), 2, $0) ++
             string:right(integer_to_list(Ms), 3, $0)).

-endif. %% -ifdef(QC).
