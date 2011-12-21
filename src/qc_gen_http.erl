%%%-------------------------------------------------------
%%% Copyright (c) 2008-2011 Gemini Mobile Technologies, Inc.
%%% All rights reserved.
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

-compile(export_all).

-include("qc_impl.hrl").

-ifdef(QC).


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

gen_entity_tag() ->
    ?LET(OptionalWeak, oneof([?WEAK, ""]),
	 ?LET(OT, gen_opaque_tag(),
	      OptionalWeak++OT)).

gen_opaque_tag() ->
    gen_quoted_string().

gen_quoted_string() ->
    ?LET(S0,
	 list(frequency([{100,gen_qdtext()},
			 {1,gen_quoted_pair()}])),
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
	 ?LET(Head, oneof([?SP,?HT]),
	      ?LET(X, list(oneof([?SP,?HT])),
		   OptionalCRLF++[Head|X]))).
		      
    


-endif. %% -ifdef(QC).
