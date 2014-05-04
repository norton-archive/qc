%%%-------------------------------------------------------
%%% Copyright (C) 2013-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
%%% Copyright (c) 2008-2012 Gemini Mobile Technologies, Inc.  All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0
%%% (the "License");
%%% you may not use this file except in compliance with
%%% the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software
%%% distributed under the License is distributed on an
%%% "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
%%% either express or implied.
%%% See the License for the specific language governing
%%% permissions and
%%% limitations under the License.
%%%-------------------------------------------------------

-module(qc_gen_http).

-ifdef(QC).

-compile(export_all).

-include("qc_impl.hrl").


%% ---rfc2616.txt-------
%%
%% entity-tag = [ weak ] opaque-tag
%% weak       = "W/"
%% opaque-tag = quoted-string
%%
%% quoted-string  = ( <"> *(qdtext | quoted-pair ) <"> )
%% qdtext         = <any TEXT except <">>
%% quoted-pair    = "\" CHAR
%%
%% CHAR           = <any US-ASCII character (octets 0 - 127)>
%%
%% TEXT           = <any OCTET except CTLs, but including LWS>
%% LWS            = [CRLF] 1*( SP | HT )
%%
%% CTL            = <any US-ASCII control character
%%                  (octets 0 - 31) and DEL (127)>
%%
%% CR             = <US-ASCII CR, carriage return (13)>
%% LF             = <US-ASCII LF, linefeed (10)>
%% SP             = <US-ASCII SP, space (32)>
%% HT             = <US-ASCII HT, horizontal-tab (9)>
%% <">            = <US-ASCII double-quote mark (34)>
%%

-define(CR, 13).
-define(LF, 10).
-define(SP, 32).
-define(HT, 9).
-define(WEAK, "W/").

-define(DEFAULT_QD, 1).
-define(DEFAULT_QP, 1).

gen_entity_tag() ->
    gen_entity_tag(?DEFAULT_QD, ?DEFAULT_QP).
gen_entity_tag(QD,QP) ->
    ?LET(OptionalWeak, oneof([?WEAK, ""]),
         ?LET(OT, gen_opaque_tag(QD,QP),
              OptionalWeak++OT)).

gen_opaque_tag() ->
    gen_opaque_tag(?DEFAULT_QD, ?DEFAULT_QP).
gen_opaque_tag(QD,QP) ->
    gen_quoted_string(QD,QP).

gen_quoted_string() ->
    gen_quoted_string(?DEFAULT_QD, ?DEFAULT_QP).
gen_quoted_string(QD,QP) ->
    ?LET(S0,
         list(frequency([{QD,gen_qdtext()},
                         {QP,gen_quoted_pair()}])),
         [$"|S0]++"\"").

gen_qdtext() ->
    ?SUCHTHAT(Chr, gen_text(), Chr =/= $").

gen_quoted_pair() ->
    ?LET(Chr, gen_char(), [$\\, Chr]).

gen_char() ->
    choose(0, 127).

gen_text() ->
    oneof([choose(32,126), gen_lws()]).

gen_lws() ->
    ?LET(OptionalCRLF, oneof([[?CR,?LF], []]),
         ?LET(SPHT, at_least(1, oneof([?SP,?HT])),
              OptionalCRLF++SPHT)).

%% --- rfc2616.txt --- 14.35 Range ---
%% bytes-unit       = "bytes"
%% DIGIT          = <any US-ASCII digit "0".."9">
%%
%% ranges-specifier = byte-ranges-specifier
%% byte-ranges-specifier = bytes-unit "=" byte-range-set
%% byte-range-set  = 1#( byte-range-spec | suffix-byte-range-spec )
%% byte-range-spec = first-byte-pos "-" [last-byte-pos]
%% first-byte-pos  = 1*DIGIT
%% last-byte-pos   = 1*DIGIT

%% suffix-byte-range-spec = "-" suffix-length
%% suffix-length = 1*DIGIT

gen_byte_unit() ->
    "bytes".
gen_digit() ->
    oneof([$0, $1, $2, $3, $4, $5, $6, $7, $8, $9]).

gen_ranges_specifier(0) ->
    undefined;
gen_ranges_specifier(Size) ->
    gen_byte_ranges_specifier(Size).

gen_ranges_specifier() ->
    gen_byte_ranges_specifier().

gen_byte_ranges_specifier(Size) ->
    ?LET(BU, gen_byte_unit(),
         ?LET(BRS, gen_byte_range_set(Size),
              BU++[$=|BRS])).

gen_byte_ranges_specifier() ->
    ?LET(BU, gen_byte_unit(),
         ?LET(BRS, gen_byte_range_set(),
              BU++[$=|BRS])).

gen_byte_range_set(Size) ->
    %% todo: multiple range
    oneof([gen_byte_range_spec(Size),
           gen_suffix_byte_range_spec(Size)]).

gen_byte_range_set() ->
    at_least_csl(1, oneof([gen_byte_range_spec(),
                           gen_suffix_byte_range_spec()])).

gen_byte_range_spec(Size) ->
    ?LET(FBP, choose(0, Size - 1),
         ?LET(LBP, choose(FBP, Size - 1),
              ?LET(LBP2, oneof(["", integer_to_list(LBP)]),
                   integer_to_list(FBP) ++ [$-|LBP2]))).

gen_byte_range_spec() ->
    ?LET(FBP, gen_first_byte_pos(),
         ?LET(LBP, oneof([gen_last_byte_pos(), ""]),
              FBP++[$-|LBP])).

gen_first_byte_pos() ->
    at_least(1, gen_digit()).

gen_last_byte_pos() ->
    at_least(1, gen_digit()).

gen_suffix_byte_range_spec(Size) ->
    ?LET(SL, choose(1, Size),
         [$-|integer_to_list(SL)]).

gen_suffix_byte_range_spec() ->
    ?LET(SL, gen_suffix_length(),
         [$-|SL]).

gen_suffix_length() ->
    at_least(1, gen_digit()).

%% --- utils ---
at_least(1, Gen) -> %% todo: N elements at least
    ?LET(Head, Gen,
         ?LET(Tail, list(Gen),
              [Head|Tail])).

at_least_csl(N, Gen) ->
    %% for comma seprated list --- "<n>#<m>element"
    ?LET(List, at_least(N, Gen),
         string:join(List, ", ")). %% todo: *LWS


-endif. %% -ifdef(QC).
