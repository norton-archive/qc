%%%----------------------------------------------------------------------
%%% Copyright (c) 2009-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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

-export([classify/1, classify2/1, classify2/2, valid8/1, force_to_utf8/1]).

%%
%% Exported Function Specs
%%

-spec classify(In::binary()) -> ascii | jis | utf8 | '8bit'.
-spec classify2(In::binary()) -> {'7bit'|'8bit'|'iso-2022'|'utf8', non_neg_integer()}.
-spec classify2(In::binary(), Max::pos_integer()|undefined) -> {'7bit'|'8bit'|'iso-2022'|'utf8', non_neg_integer()}.
-spec valid8(In::binary()) -> {true, binary()} | false | fuzzy.
-spec force_to_utf8(In::binary()) -> binary().

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
%% 'ascii' (7bit), 'jis' (7bit) or 'utf8' or '8bit'. Classification
%% is strict and fails quickly to 8bit if an 8bit byte is found.
%% @see classify2/2 for a less strict classifier.
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
        <<2#111100:6,_/bits>> ->
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

%%
-record(state,
        { seven = 0 :: non_neg_integer()  %% count of 7bit bytes
        , eight = 0 :: non_neg_integer()  %% count of 8bit bytes (not belonging to a utf8 char)
        , utf8  = 0 :: non_neg_integer()  %% count of complete utf8 characters
        , esc   = 0 :: non_neg_integer()  %% count of iso-2022 escape sequences (2bytes ^$,^(,^- counted as 1)
        }).


%% @doc classify2 runs through a binary byte-by-byte and tries to classify
%% if as either '7bit','8bit','iso-2022' or 'utf8'. For big data a max number
%% of bytes can be specified to reduce the overhead. Classification allows
%% for a certain amount of misc-bytes in the data so for example a bad byte
%% in what is otherwise perfect utf-8 will still be classified as utf8.
classify2(In) ->
    classify2(In, undefined).
classify2(In, undefined) ->
    classify2a(In, #state{});
classify2(In, Max) when is_integer(Max) ->
    case In of
        <<Prefix:Max/binary,_/binary>> ->
            classify2a(Prefix, #state{});
        _ ->
            classify2a(In, #state{})
    end.

%% No Input left, all done!
classify2a(<<>>, State) ->
    classify2_finalize(State);

%% Escape Sequences
classify2a(<<27,36,T/binary>>, #state{esc=C}=State) ->
    classify2a(T, State#state{esc=C+1}); %% ^$
classify2a(<<27,40,T/binary>>, #state{esc=C}=State) ->
    classify2a(T, State#state{esc=C+1}); %% ^(
classify2a(<<27,45,T/binary>>, #state{esc=C}=State) ->
    classify2a(T, State#state{esc=C+1}); %% ^-

%% 7bit Bytes caught here so not counted as utf8
classify2a(<<0:1,_:7/bits,T/binary>>, #state{seven=S}=State) ->
    classify2a(T, State#state{seven=S+1});

%% Otherwise its utf8 or 8bit
classify2a(<<_H,T/binary>>=In, #state{utf8=U,eight=E}=State) ->
    case valid8(In) of
        {true, NextT} ->
            classify2a(NextT, State#state{utf8=U+1});
        fuzzy when U > 0, E =:= 0 ->
            %% if other utf8 chars, then likely its utf8
            %% and that makes it, end of stream too.
            classify2_finalize(State#state{utf8=U+1});
        fuzzy ->
            %% Count as 8bit, not sure if more bytes so lets keep going
            classify2a(T, State#state{eight=E+1});
        false ->
            classify2a(T, State#state{eight=E+1})
    end.

%% @doc Once the byte input has properly counted, we can finalize
%% the classification by choosing which class the input belongs to
%% based on different counts. This classification is based on the
%% fact that outside of utf8 it should be hard to create a proper
%% utf8 character by accident and outside of iso-2022-jp it should
%% be hard to have a proper escape sequence. It's not perfect.
classify2_finalize(#state{utf8=1,eight=0}) ->
    {'utf8', 0};
classify2_finalize(#state{utf8=1,eight=1}) ->
    {'8bit', 1};
classify2_finalize(#state{utf8=U,eight=E}) when U > 0 ->
    if
        E > 1 andalso U < 11 ->
            {'8bit', E}; %% allow 1 8bit for 10 or less
        E > 2 andalso U > 10 andalso U < 101 ->
            {'8bit', E}; %% allow 2 8bit for 10 to 100 chars
        E > round(U*0.02) andalso U > 100 ->
            {'8bit', E}; %% only allow 2% 8bit errors
        true ->
            {'utf8', E} %% none or acceptable amnt of 8bit errors
    end;
classify2_finalize(#state{esc=I,eight=E}) when I > 0 ->
    {'iso-2022', E};
classify2_finalize(#state{eight=E}) when E > 0 ->
    {'8bit', E};
classify2_finalize(#state{eight=E}) ->
    {'7bit', E}.

%% @doc Checks if the Input starts with a valid utf8 character. If it is
%% utf8 it returns true and a binary starting from the next byte following
%% that utf8 character. If it is not utf8, then it returns false. If the
%% stream ends with a truncated potential utf-8, it returns 'fuzzy' as its
%% not certain if it would be a utf-8 character if we had more data or not.
valid8(<<>>) ->
    false;
valid8(<<H,T/binary>>) ->
    case <<H>> of
        <<0:1,_/bits>> ->
            {true, T};          % 1 byte; normal
        <<2#110:3,_/bits>> when (H band 2#111110) =/= 0 ->   % Disallow C0 or C1
            valid8(T, 1);       % 2 byte; normal
        <<16#E0>> ->
            valid8(T, 2, sp1);  % 3 byte; special 1
        <<16#ED>> ->
            valid8(T, 2, sp2);  % 3 byte; special 2
        <<16#EF>> ->
            valid8(T, 2, sp5);  % 3 byte; special 5 (for non-chars)
        <<2#1110:4,_/bits>> ->
            valid8(T, 2);       % 3 byte; normal
        <<16#F0>> ->
            valid8(T, 3, sp3);  % 4 byte; special 3
        <<16#F4>> ->
            valid8(T, 3, sp4);  % 4 byte; special 4
        <<2#111100:6,_/bits>> ->
            valid8(T, 3);       % 4 byte normal
        _ ->
            false               % not utf-8;
    end.

valid8(In, 0) ->
    {true, In};   %% complete, valid utf8 found. Next is In.
valid8(<<>>, _) ->
    fuzzy;        %% incomplete. not sure if utf8 or not
valid8(<<2#10:2,_:6/bits,T/binary>>, ByteCnt) ->
    valid8(T, ByteCnt-1);  %% valid continuation byte
valid8(_, _) ->
    false.        %% invalid utf8 continuation byte

%% Special cases (mostly for second bytes)
valid8(<<>>, _, _) ->
    fuzzy;        %% incomplete. not sure if utf8 or not
valid8(<<H,T/binary>>, ByteCnt, sp1) when H >= 16#A0, H =< 16#BF ->
    valid8(T, ByteCnt-1);
valid8(<<H,T/binary>>, ByteCnt, sp2) when H >= 16#80, H =< 16#9F ->
    valid8(T, ByteCnt-1);
valid8(<<H,T/binary>>, ByteCnt, sp3) when H >= 16#90, H =< 16#BF ->
    valid8(T, ByteCnt-1);
valid8(<<H,T/binary>>, ByteCnt, sp4) when H >= 16#80, H =< 16#8F ->
    valid8(T, ByteCnt-1);
%% Special for catching unicode non-characters EFBFBE and EFBFBF (FFFE and FFFF)
valid8(<<16#BF,T/binary>>, 2, sp5) ->
    valid8(T, 1, sp5);
valid8(<<H,_/binary>>, 1, sp5) when H =:= 16#BE; H =:= 16#BF ->
    false;
valid8(In, ByteCnt, sp5) ->
    valid8(In, ByteCnt); % classify remaining bytes normally
valid8(_, _, _) ->
    false.

%% This function takes a binary input which is supposed to be utf8 and checks
%% that it indeed is utf8. Any 8bit bytes which don't conform to utf8 including
%% trailing bytes (perhaps truncated data), will be mapped to the '?' character.
%% This function returns utf8 output (hopefully same as input).
force_to_utf8(In) ->
    case force_to_utf8(In, []) of
        [] ->
            In; %% usual case returns the input as-is.
        BSList ->
            fix_utf8(In, BSList, [])
    end.

%% pass through Input looking for bad bytes and remember a list sizes
%% where the input was bad. This is optimized for the case where there
%% is nothing to do, rather than trying to collect parts of the
%% binary here.
force_to_utf8(<<>>, List) ->
    lists:reverse(List);
force_to_utf8(<<_H,T/binary>>=In, List) ->
    case valid8(In) of
        {true, NextT} ->
            force_to_utf8(NextT, List);
        _ ->
            force_to_utf8(T, [size(In)|List])
    end.

%% Process the input and replace bytes indicated in the BSList
%% with the '?' character. The BSList is a list of binary sizes
%% when a bad 8bit byte was found. Ie, if the size of In is 7
%% and the BSList is [7,3], then replace bytes at 0 and 4 with
%% the '?' character.
fix_utf8(<<>>, [], IOList) ->
    list_to_binary(lists:reverse(IOList));
fix_utf8(In, [], IOList) ->
    fix_utf8(<<>>, [], [In|IOList]);
fix_utf8(In, [H|T], IOList) ->
    PrefixLen = size(In) - H,
    case In of
        <<_Bad,InMore/binary>> when PrefixLen =:= 0 ->
            fix_utf8(InMore, T, [<<$?>>|IOList]);
        <<Prefix:PrefixLen/binary,_Bad,InMore/binary>> ->
            fix_utf8(InMore, T, [<<$?>>|[Prefix|IOList]])
    end.
