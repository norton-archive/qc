%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gmt_eqc_gen.erl
%%% Purpose : GMT QuickCheck generators
%%%-------------------------------------------------------------------

-module(gmt_eqc_gen).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).


%% helper for sizing down recursive generators
-define(SIZEDOWN(G),
        ?SIZED(Size, eqc_gen:resize(round(math:sqrt(Size)),G))).


%%%-------------------------------------------------------------------
%%% Base Generators
%%%


%%%%%%
%% atom
gmt_atom() ->
    gmt_atom([]).

gmt_atom(Attrs) when is_list(Attrs) ->
    X = lists:usort(Attrs),
    StringAttrs = [ Attr || Attr <- X, Attr =/= nonundefined ],
    eqc_gen:oneof([ok,true,false,?LET(L,gmt_string(StringAttrs),list_to_atom(L))]
                  ++ if StringAttrs =:= X -> [undefined]; true -> [] end).


%%%%%%
%% binary
gmt_binary() ->
    gmt_binary([]).

gmt_binary(Attrs) when is_list(Attrs) ->
    ?LET(L,gmt_string(Attrs),list_to_binary(L)).



%%%%%%
%% proplist
gmt_proplist() ->
    gmt_proplist([]).

gmt_proplist(Attrs) when is_list(Attrs) ->
    Key = gmt_atom(),
    Val = gmt_any(),
    G = {Key, Val},
    gmt_list(G, Attrs).


%%%%%%
%% list
gmt_list() ->
    G = gmt_any(),
    gmt_list(G).

gmt_list(G) ->
    gmt_list(G, []).

gmt_list(G, Attrs) when is_list(Attrs) ->
    case lists:usort(Attrs) of
        [] ->
            eqc_gen:list(G);
        [nonempty] ->
            ?LET(L,eqc_gen:list(G), if L =/= [] -> L; true -> [G] end)
    end.

%%%%%%
%% string
gmt_string() ->
    gmt_string([]).

gmt_string(Attrs) when is_list(Attrs) ->
    case lists:usort(Attrs) of
        [] ->
            eqc_gen:oneof([eqc_gen:list(char())
                           ,  ?LET(Attrs1,eqc_gen:oneof([[nonempty], [ascii], [ascii,nonempty], [asciiprintable], [asciiprintable,nonempty], [ascii_alphanumeric], [ascii_alphanumeric, nonempty]])
                                   , gmt_string(Attrs1))
                          ]);
        [nonempty] ->
            gmt_list(char(),[nonempty]);
        [CharAttr] ->
            eqc_gen:list(gmt_char([CharAttr]));
        [CharAttr, nonempty] when is_atom(CharAttr) ->
            gmt_list(gmt_char([CharAttr]),[nonempty])
    end.


%%%%%%
%% term
gmt_term() ->
    gmt_term([]).

gmt_term(Attrs) ->
    gmt_any(Attrs).


%%%%%%
%% tuple
gmt_tuple() ->
    gmt_tuple([]).

gmt_tuple(Attrs) when is_list(Attrs) ->
    G = gmt_any(),
    ?LET(L,gmt_list(G, Attrs),
         list_to_tuple(L)).


%%%-------------------------------------------------------------------
%%% Other Generators
%%%


%%%%%
%% any
gmt_any() ->
    gmt_any([]).

gmt_any(Attrs) ->
    ?SIZEDOWN(
       case X = lists:usort(Attrs) of
           [] ->
               %% @todo must be a better definition!!!
               eqc_gen:oneof([
                              gmt_atom(), gmt_string(), gmt_binary(), gmt_tuple(), gmt_list(), gmt_proplist(), gmt_term()
                              ,  ?LET(Attrs1,eqc_gen:oneof([[nonempty], [nonundefined], [nonempty,nonundefined]])
                                      , gmt_any(Attrs1))
                             ]);
           [nonempty] ->
               eqc_gen:oneof([gmt_atom(X), gmt_string(X), gmt_binary(X), gmt_tuple(X), gmt_list(X), gmt_proplist(X), gmt_term(X)]);
           [nonundefined] ->
               eqc_gen:oneof([gmt_atom(X), gmt_string(), gmt_binary(), gmt_tuple(), gmt_list(), gmt_proplist(), gmt_term(X)]);
           [nonempty,nonundefined] ->
               Y = [nonempty],
               eqc_gen:oneof([gmt_atom(X), gmt_string(Y), gmt_binary(Y), gmt_tuple(Y), gmt_list(Y), gmt_proplist(Y), gmt_term(X)])
       end).


%%%%%%
%% char
gmt_char(Attrs) when is_list(Attrs) ->
    case lists:usort(Attrs) of
        [ascii] ->
            eqc_gen:choose(0,127);
        [asciiprintable] ->
            eqc_gen:choose(32,126);
        [ascii_alphanumeric] ->
            eqc_gen:oneof([
                           eqc_gen:choose(65, 90),  % A - Z
                           eqc_gen:choose(97, 122), % a - z
                           eqc_gen:choose(48, 57)   % 0 - 9
                          ])
    end.


%%%%%%
%% timeout - avoid a real timeout by starting from 100 sec
gmt_timeout() ->
    eqc_gen:oneof([infinity,eqc_gen:choose(100000,10000000)]).


%%%%%%
%% expires
gmt_expires() ->
    ?LET(Timeout,gmt_timeout(),gmt_time:make_expires(Timeout)).


%%%%%%
%% timeout_or_expires
gmt_timeout_or_expires() ->
    ?LET(Timeout,gmt_timeout(),eqc_gen:oneof([Timeout,gmt_time:make_expires(Timeout)])).


%%%%%%
%% Bool
%% @spec syntactic sugar for eqc_gen:bool(), true/false value could be set
%%         thru proplist Attrs. (Also a better way for gmt_choose if there are
%%         only 2 items) For examples:
%%         Attrs = [{true, 1}, {false, 0}].
%%         Attrs = [{true, "1"}, {false, "0"}]
%%         Attrs = [{true, "true"}, {false, "false"}]
%%         Attrs = [{true, "dog"}, {false, "cat"}]
%%         Attrs = [{true, ok}, {false, ng}]
gmt_bool(Attrs) ->
    ?LET(Bool, bool(), proplists:get_value(Bool, Attrs, Bool)).

gmt_bool(True, False) ->
    gmt_bool([{true, True}, {false, False}]).


%%%%%%
%% integer
%% TODO(gki): make attr: [length, min, max, base]
gmt_int_list(Length, Base) ->
    Range = round(math:pow(Base, Length)) - 1, % int() is too small
    ?SUCHTHAT(L,
              ?LET(I, choose(0, Range), string:right(erlang:integer_to_list(abs(I), Base), Length, $0)),
              length(L) =:= Length).


%%%%%%
%% varchar
%% varchar() -> random size nonempty ascii aphanumeric string
%% varchar(S) -> fixed size nonempty ascii aphanumeric string
%% varchar(Min, Max) -> ranged nonempty ascii aphanumeric string
gmt_varchar() ->
    gmt_string([ascii_alphanumeric, nonempty]).

gmt_varchar(0) ->
    [];
gmt_varchar(S) when is_integer(S) ->
    gmt_varchar(S, S).

gmt_varchar(Min, Max) when is_integer(Min) andalso is_integer(Max) ->
    gmt_varchar(Min, Max, []).

gmt_varchar(_, Max, Buffer) when length(Buffer) > Max ->
    string:substr(Buffer, 1, Max);
gmt_varchar(Min, Max, Buffer) when length(Buffer) < Min ->
    ?LET(S, gmt_varchar(), gmt_varchar(Min, Max, Buffer ++ S));
gmt_varchar(_, _, Buffer) ->
    Buffer.


%%%%%%
%% time
gmt_date_yymmdd() ->
    ?LET({Y,M}, {choose(0, 99), choose(1, 12)}, gmt_date_yymmdd(Y, M)).

gmt_date_yymmdd(Y, M) when is_integer(Y) andalso is_integer(M) ->
    ?LET(D,
         oneof(
           [choose(1, 28) || (M=:=2) and not calendar:is_leap_year(Y)] ++
               [choose(1, 29) || (M=:=2) and calendar:is_leap_year(Y)] ++
               [choose(1, 30) || lists:member(M, [4,6,9,11])] ++
               [choose(1, 31) || lists:member(M, [1,3,5,7,8,10,12])]),
         string:right(integer_to_list(Y), 2, $0) ++
             string:right(integer_to_list(M), 2, $0) ++
             string:right(integer_to_list(D), 2, $0)
        ).


gmt_time_hhmmss() ->
    ?LET({H,M,S}, {choose(0, 23), choose(0, 59), choose(0, 59)},
         string:right(integer_to_list(H), 2, $0) ++
             string:right(integer_to_list(M), 2, $0) ++
             string:right(integer_to_list(S), 2, $0)).


gmt_time_hhmmssms() ->
    ?LET({H,M,S,Ms}, {choose(0, 23), choose(0, 59), choose(0, 59), choose(0, 999)},
         string:right(integer_to_list(H), 2, $0) ++
             string:right(integer_to_list(M), 2, $0) ++
             string:right(integer_to_list(S), 2, $0) ++
             string:right(integer_to_list(Ms), 3, $0)).

-endif. %% -ifdef(EQC).
