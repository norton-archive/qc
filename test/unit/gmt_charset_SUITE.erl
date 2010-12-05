%%%-------------------------------------------------------------------
%%% $Id$
%%% Description: GMT charset test suite
%%% Copyright: (c) 2010 Gemini Mobile Technologies, Inc.  All rights reserved.
%%%-------------------------------------------------------------------

-module(gmt_charset_SUITE).

-export([start_test/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Test Cases
%%
-spec start_test() -> ok.
start_test() ->
    ok = start_1_000_test(),
    ok = start_2_000_test(),
    ok = start_3_000_test(),
    ok = start_4_000_test(),
    ok = start_5_000_test(),
    ok = start_6_000_test(),
    ok = start_7_000_test(),
    ok = start_8_000_test(),
    ok.

start_1_000_test() ->
    %% valid8 on empty binaries
    false = gmt_charset:valid8(<<>>),
    %% validate 7bit data range
    {true, <<>>} = gmt_charset:valid8(<<16#00>>),
    {true, <<>>} = gmt_charset:valid8(<<16#7F>>),
    %% lone utf-8 tail is not utf-8
    false = gmt_charset:valid8(<<16#80>>),
    false = gmt_charset:valid8(<<16#BF>>),
    %% C0 and C1 are also invalid first bytes
    false = gmt_charset:valid8(<<16#C0>>),
    false = gmt_charset:valid8(<<16#C1>>),
    %% F4 should be fuzzy
    fuzzy = gmt_charset:valid8(<<16#F4>>),
    %% F5 to FF are also invalid first bytes
    false = gmt_charset:valid8(<<16#F5>>),
    false = gmt_charset:valid8(<<16#FF>>),
    ok.

start_2_000_test() ->
    %% check truncated 2-byte utf-8
    fuzzy = gmt_charset:valid8(<<16#C2>>),
    %% check truncated 3-byte utf-8
    fuzzy = gmt_charset:valid8(<<16#E0,16#A9>>),
    fuzzy = gmt_charset:valid8(<<16#E0>>),
    %% check truncated 4-byte utf-8
    fuzzy = gmt_charset:valid8(<<16#F3,16#BF,16#BF>>),
    fuzzy = gmt_charset:valid8(<<16#F3,16#BF>>),
    fuzzy = gmt_charset:valid8(<<16#F3>>),
    ok.

start_3_000_test() ->
    %% 2 non-utf-8 chars explicitly dis-allowed
    false = gmt_charset:valid8(<<16#EF,16#BF,16#BE>>),
    false = gmt_charset:valid8(<<16#EF,16#BF,16#BF>>),
    %% and some close by chars
    {true, <<>>} = gmt_charset:valid8(<<16#EF,16#BF,16#BD>>),
    {true, <<>>} = gmt_charset:valid8(<<16#EF,16#BE,16#BF>>),
    ok.

start_4_000_test() ->
    %% 1 byte chars followed by 8bit data
    {true, <<16#FE,16#FF>>} = gmt_charset:valid8(<<$A,16#FE,16#FF>>),
    {true, <<$B,16#FE,16#FF>>} = gmt_charset:valid8(<<$A,$B,16#FE,16#FF>>),
    %% 2 byte chars followed by 8bit data
    {true, <<16#FE,16#FF>>} = gmt_charset:valid8(<<16#C3,16#B6,16#FE,16#FF>>),
    %% 3 byte chars followed by 8bit data
    {true, <<16#FE,16#FF>>} = gmt_charset:valid8(<<16#EA,16#A2,16#AC,16#FE,16#FF>>),
    %% 4 byte chars followed by 8bit data
    {true, <<16#FE,16#FF>>} = gmt_charset:valid8(<<16#F0,16#90,16#92,16#80,16#FE,16#FF>>),
    ok.

start_5_000_test() ->
    %% mostly 7bit data tests
    {'7bit', 0} = gmt_charset:classify2(<<>>),
    {'7bit', 0} = gmt_charset:classify2(<<>>, undefined),
    {'7bit', 0} = gmt_charset:classify2(<<>>, 500),
    {'7bit', 0} = gmt_charset:classify2(<<"hello there">>),
    {'7bit', 0} = gmt_charset:classify2(<<"hello",16#7F>>, 6),
    {'8bit', 1} = gmt_charset:classify2(<<"hello",16#8F>>, 6),
    {'utf8', 0} = gmt_charset:classify2(<<"hello",16#C2,16#80>>, 7),  %% full utf-8 character
    {'8bit', 1} = gmt_charset:classify2(<<"hello",16#C2,16#80>>, 6),  %% truncated and no other utf-8
    {'iso-2022', 0} = gmt_charset:classify2(<<"hello",16#1B,$$>>, 7),
    {'7bit', 0} = gmt_charset:classify2(<<"hello",16#1B,$$>>, 6),
    {'7bit', 0} = gmt_charset:classify2(<<"hello",16#8F>>, 5),
    {'7bit', 0} = gmt_charset:classify2(<<"hello",16#E0>>, 5),
    {'7bit', 0} = gmt_charset:classify2(<<"hello",16#1B,$$>>, 5),
    ok.

start_6_000_test() ->
    %% utf8 nihongo
    {'utf8', 0} = gmt_charset:classify2(<<16#e6,16#97,16#a5,16#e6,16#9c,16#ac,16#e8,16#aa,16#9e>>),
    %% utf8 koibito
    {'utf8', 0} = gmt_charset:classify2(<<16#e6,16#81,16#8b,16#e4,16#ba,16#ba>>),
    %% shift_jis nihongo... 1x7bit and last byte could be start of truncated utf-8, but no others so 5.
    {'8bit', 5} = gmt_charset:classify2(<<16#93,16#fa,16#96,16#7b,16#8c,16#ea>>),
    {'iso-2022', 0} = gmt_charset:classify2(<<16#1b,16#24,16#42,16#46,16#7c,16#4b,16#5c,16#38,16#6c,16#1b,16#28,16#42>>),
    %% koibito in shift_jis
    {'8bit', 3} = gmt_charset:classify2(<<16#97,16#f6,16#90,16#6c>>), % 1x7bit
    %% koibito in euc-jp
    {'8bit', 4} = gmt_charset:classify2(<<16#ce,16#f8,16#bf,16#cd>>),
    %% utf8 nih (snipped short)
    {'utf8', 0} = gmt_charset:classify2(<<16#e6,16#97,16#a5,16#e6,16#9c>>),
    %% utf8 nih (snipped shorter)
    {'utf8', 0} = gmt_charset:classify2(<<16#e6,16#97,16#a5,16#e6>>),
    %% utf8 ni (with an invalid 8bit byte)
    {'8bit', 1} = gmt_charset:classify2(<<16#e6,16#97,16#a5,16#f5>>),
    %% utf8 ni (with 2 invalid 8bit bytes)
    {'8bit', 2} = gmt_charset:classify2(<<16#C3,16#e6,16#97,16#a5,16#f5>>),
    ok.

start_7_000_test() ->
    %% chinese in gb18030, 39 bytes; 5x7bit; 10xutf8; 14x8bit - hmmm many validate as utf-8!
    {'8bit', 14} = gmt_charset:classify2(<<16#54,16#4f,16#4d,16#cd,16#f8,16#ce,16#aa,16#d3,16#c3,16#bb,16#a7,16#32,16#34,16#d0,16#a1,16#ca,16#b1,16#cc,16#e1,16#b9,16#a9,16#c8,16#ab,16#c3,16#e6,16#bc,16#b0,16#ca,16#b1,16#b5,16#c4,16#d6,16#d0,16#ce,16#c4,16#d7,16#ca,16#d1,16#b6>>),

    %% chinese in big5. 71 bytes; 32x7bit; 1xutf8; 37x8bit
    {'8bit', 37} = gmt_charset:classify2(<<16#af,16#ab,16#20,16#ba,16#d9,16#20,16#a5,16#fa,16#20,16#ac,16#b0,16#20,16#b1,16#de,16#20,16#a1,16#42,16#20,16#ba,16#d9,16#20,16#b7,16#74,16#20,16#ac,16#b0,16#20,16#a9,16#5d,16#20,16#a1,16#44,16#20,16#a6,16#b3,16#20,16#b1,16#df,16#20,16#a4,16#57,16#20,16#a1,16#42,16#20,16#a6,16#b3,16#20,16#a6,16#ad,16#20,16#b1,16#e1,16#20,16#a1,16#42,16#20,16#b3,16#6f,16#20,16#ac,16#4f,16#20,16#c0,16#59,16#20,16#a4,16#40,16#20,16#a4,16#e9>>),

    %% korean in euc-kr; 63 bytes; 10x7bit; 14xutf8; 33x8bit;
    {'8bit', 33} = gmt_charset:classify2(<<16#c7,16#d1,16#b1,16#b9,16#bf,16#dc,16#b1,16#b9,16#be,16#ee,16#b4,16#eb,16#c7,16#d0,16#b1,16#b3,16#20,16#b0,16#e6,16#bf,16#b5,16#b4,16#eb,16#c7,16#d0,16#bf,16#f8,16#69,16#66,16#4d,16#42,16#41,16#2c,16#20,16#c7,16#d1,16#b1,16#b9,16#bf,16#dc,16#b4,16#eb,16#20,16#b1,16#b9,16#c1,16#a6,16#20,16#b1,16#dd,16#c0,16#b6,16#c7,16#d0,16#b0,16#fa,16#20,16#bf,16#c2,16#b6,16#f3,16#c0,16#ce>>),

    %% polish in iso-8859-2, mostly 7bit with some 8bit mixed in
    {'8bit', 4} = gmt_charset:classify2(<<16#4e,16#61,16#20,16#62,16#69,16#65,16#bf,16#b1,16#63,16#6f,16#20,16#61,16#6b,16#74,16#75,16#61,16#6c,16#69,16#7a,16#6f,16#77,16#61,16#6e,16#65,16#20,16#69,16#6e,16#66,16#6f,16#72,16#6d,16#61,16#63,16#6a,16#65,16#20,16#7a,16#20,16#70,16#6f,16#6c,16#69,16#74,16#79,16#6b,16#69,16#2c,16#20,16#77,16#79,16#64,16#61,16#72,16#7a,16#65,16#f1,16#20,16#77,16#20,16#6b,16#72,16#61,16#6a,16#75,16#20,16#69,16#20,16#7a,16#61,16#67,16#72,16#61,16#6e,16#69,16#63,16#b1>>),

    %% hebrew in iso-8859-8; 36 bytes; 6x7bit; 30x8bit (high bit heavy code)
    {'8bit', 30} = gmt_charset:classify2(<<16#f4,16#e9,16#f7,16#e5,16#e3,16#20,16#e4,16#f2,16#e5,16#f8,16#f3,16#3a,16#20,16#fa,16#e4,16#e9,16#e5,16#20,16#ee,16#e5,16#eb,16#f0,16#e9,16#ed,16#20,16#ec,16#e9,16#fa,16#f8,16#20,16#e1,16#e9,16#e8,16#e7,16#e5,16#ef>>),

    %% Japanese (in shift_jis) ; 34 bytes; 7x7bit; 17x8bit; 5xutf-8
    {'8bit', 17} = gmt_charset:classify2(<<16#52,16#45,16#3a,16#20,16#81,16#79,16#89,16#a9,16#8b,16#e0,16#82,16#cc,16#97,16#a2,16#81,16#7a,16#82,16#a8,16#93,16#cd,16#82,16#af,16#93,16#fa,16#82,16#c9,16#82,16#c2,16#82,16#a2,16#82,16#c4,16#81,16#42>>),
    
    %% hiragana only in shift_jis; 10 bytes; 3xutf8; 1x7bit; 3x8bit
    {'8bit', 3} = gmt_charset:classify2(<<16#82,16#c9,16#82,16#c2,16#82,16#a2,16#82,16#c4,16#81,16#42>>),

    %% short hiragana only in shift_jis; 4 bytes; 1xutf8; 1x7bit; 1x8bit
    {'8bit', 1} = gmt_charset:classify2(<<16#82,16#c4,16#81,16#42>>),

    %% 1/2 width katakana in iso-2022-jp-2
    {'iso-2022',0} = gmt_charset:classify2(<<16#1b,16#28,16#49,16#36,16#40,16#36,16#45,16#1b,16#28,16#42>>),

    %% 1/2 width katakana in shift_jis
    {'8bit',4} = gmt_charset:classify2(<<16#b6,16#c0,16#b6,16#c5>>),

    %% 1/2 width katakana in euc-jp; 8 bytes; 8x8bit; 
    {'8bit',8} = gmt_charset:classify2(<<16#8e,16#b6,16#8e,16#c0,16#8e,16#b6,16#8e,16#c5>>),

    %% 1/2 width katakana in utf8; 8 bytes; 4xutf8
    {'utf8',0} = gmt_charset:classify2(<<16#ef,16#bd,16#b6,16#ef,16#be,16#80,16#ef,16#bd,16#b6,16#ef,16#be,16#85>>),

    ok.

start_8_000_test() ->
    <<>> = gmt_charset:force_to_utf8(<<>>),
    <<$a>> = gmt_charset:force_to_utf8(<<$a>>),
    <<$?>> = gmt_charset:force_to_utf8(<<16#80>>),
    <<$?,16#c4,16#81,16#42>> = gmt_charset:force_to_utf8(<<16#82,16#c4,16#81,16#42>>),
    <<$?,16#c4,16#81,$?>> = gmt_charset:force_to_utf8(<<16#82,16#c4,16#81,16#FF>>),
    <<$?,$?,$?,$?>> = gmt_charset:force_to_utf8(<<16#82,16#FF,16#FF,16#FF>>),

    %% Decent mix of utf8 2 and 3 byte chars, 8bit and 7bit bytes (gb18030).
    <<16#54,16#4f,16#4d,$?,$?,16#ce,16#aa,$?,16#c3,16#bb,$?,16#32,16#34,16#d0,16#a1,16#ca,16#b1,$?,16#e1,16#b9,16#a9,16#c8,16#ab,$?,16#e6,16#bc,16#b0,16#ca,16#b1,$?,$?,$?,$?,$?,$?,$?,$?,16#d1,16#b6>> = gmt_charset:force_to_utf8(<<16#54,16#4f,16#4d,16#cd,16#f8,16#ce,16#aa,16#d3,16#c3,16#bb,16#a7,16#32,16#34,16#d0,16#a1,16#ca,16#b1,16#cc,16#e1,16#b9,16#a9,16#c8,16#ab,16#c3,16#e6,16#bc,16#b0,16#ca,16#b1,16#b5,16#c4,16#d6,16#d0,16#ce,16#c4,16#d7,16#ca,16#d1,16#b6>>),

    ok.
