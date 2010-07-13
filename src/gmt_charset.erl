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
%%% File    : gmt_charset.erl
%%% Purpose : GMT charset utilities
%%%----------------------------------------------------------------------

-module(gmt_charset).

-export([classify/1]).

%%
%% Exported Function Specs
%%

-spec classify(In::binary()) -> ascii | jis | utf8 | '8bit'.

%% UTF-8 is defined like this with exception of the extra 2 non-characters
%%
%% UTF8-octets = *( UTF8-char )
%% UTF8-char = UTF8-1 / UTF8-2 / UTF8-3 / UTF8-4
%% UTF8-1 = x00-7F
%% UTF8-2 = xC2-DF UTF8-tail
%%
%% UTF8-3 = xE0 %xA0-BF UTF8-tail / xE1-EC 2( UTF8-tail ) /
%%          xED %x80-9F UTF8-tail / xEE-EF 2( UTF8-tail )
%% UTF8-4 = xF0 %x90-BF 2( UTF8-tail ) / xF1-F3 3( UTF8-tail ) /
%%          xF4 %x80-8F 2( UTF8-tail )
%% UTF8-tail = x80-BF

%% @doc looks at the In and tries to classify the string as either
%% 'ascii' (7bit), 'jis' (7bit) or 'utf8' or '8bit'.
classify(In) ->
    classify(In, ascii).

classify(<<>>, Class) ->
    Class;
classify(<<2#1:1,_:7/bits,_/binary>> =In, Class) when Class =:= ascii; Class =:= jis ->
    classify(In, 0, utf8); % utf8 or 8bit as high bit is set
classify(<<27,T/binary>>, Class) when Class =:= ascii; Class =:= jis ->
    classify(T, jis);  % escape (27) might indicate jis
classify(<<_,T/binary>>, Class) when Class =:= ascii; Class =:= jis ->
    classify(T, Class). % continue as ascii/jis

classify(<<>>, 0, utf8) ->
    utf8;
classify(<<>>, _ByteCnt, utf8) ->
    '8bit';  % missing utf-8 bytes so classify as 8bit.
classify(<<H,T/binary>>, 0, utf8) ->
    case <<H>> of
        <<2#0:1,_/bits>> ->
            classify(T, 0, utf8); % 1 byte character
        <<2#110:3,_/bits>> when (H band 2#111110) =/= 0 ->   % Disallow C0 or C1
            classify(T, 1, utf8); % first of 2 byte
        <<16#E0>> ->
            classify(T, 2, sp1, utf8);  % 3 byte; special 1
        <<16#ED>> ->
            classify(T, 2, sp2, utf8);  % 3 byte; special 2
        <<16#EF>> ->
            classify(T, 2, sp5, utf8);  % 3 byte; special 5 (for non-chars)
        <<2#1110:4,_/bits>> ->
            classify(T, 2, utf8); % 3 byte; normal
        <<16#F0>> ->
            classify(T, 3, sp3, utf8);  % 4 byte; special 3
        <<16#F4>> ->
            classify(T, 3, sp4, utf8);  % 4 byte; special 4
        <<2#11110:5,_/bits>> ->
            classify(T, 3, utf8); % 4 byte normal
        _ ->
            '8bit' % Done - not utf-8
    end;
classify(<<2#10:2,_:6/bits,T/binary>>, ByteCnt, utf8) ->
    classify(T, ByteCnt-1, utf8);
classify(_, _, utf8) ->
    '8bit'.  % Done - not utf-8

%% Special cases (mostly for second bytes)
classify(<<H,T/binary>>, ByteCnt, sp1, utf8) when H >= 16#A0, H =< 16#BF ->
    classify(T, ByteCnt-1, utf8);
classify(<<H,T/binary>>, ByteCnt, sp2, utf8) when H >= 16#80, H =< 16#9F ->
    classify(T, ByteCnt-1, utf8);
classify(<<H,T/binary>>, ByteCnt, sp3, utf8) when H >= 16#90, H =< 16#BF ->
    classify(T, ByteCnt-1, utf8);
classify(<<H,T/binary>>, ByteCnt, sp4, utf8) when H >= 16#80, H =< 16#8F ->
    classify(T, ByteCnt-1, utf8);
%% Special for catching unicode non-characters EFBFBE and EFBFBF (FFFE and FFFF)
classify(<<16#BF,T/binary>>, 2, sp5, utf8) ->
    classify(T, 1, sp5, utf8);
classify(<<H,_/binary>>, 1, sp5, utf8) when H =:= 16#BE; H =:= 16#BF ->
    '8bit';
classify(In, ByteCnt, sp5, utf8) ->
    classify(In, ByteCnt, utf8); % classify remaining bytes normally
classify(_, _, _, _) ->
    '8bit'.
