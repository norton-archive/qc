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
%%% File    : gmt_time.erl
%%% Purpose : GMT time utilities
%%%----------------------------------------------------------------------

-module(gmt_time).

-export([time_t/0,
         time_t/1,
         time_t_msec/0,
         time_t_msec/1,
         time_t_usec/0,
         time_t_usec/1,
         time_integer/0,
         time_integer/1,
         global_time_integer/0,
         global_time_integer/1,
         next_dow_time/2,
         next_dow_time/4,
         rfc2616_to_time_t/1,
         rfc2822_to_time_t/1,
         time_t_to_rfc1123/0,
         time_t_to_rfc1123/1,
         time_t_to_rfc1123_binary/0,
         time_t_to_rfc1123_binary/1,
         time_t_to_rfc2822/0,
         time_t_to_rfc2822/1,
         utc_offset_rfc2822/2,
         utc_offset/0,
         gentime_to_gregs/1,
         gregs_to_gentime/1,
         gregs_to_gentime2/1,
         cal_to_bignum/1,
         cal_to_bignumstr/0,
         cal_to_bignumstr/1,
         smpp_to_abs/1,
         make_expires/1,
         make_expires/2,
         multiply_expires/2,
         multiply_expires/3,
         make_timeout/1,
         make_timeout/2,
         sec_timeout/1,
         minsec_timeout/2,
         min_timeout/2,
         add_timeouts/2,
         global_timestamp/0,
         global_timestamp/1,
         global_timestamp_cmp/2,
         global_timestamp_less/2,
         global_timestamp_binary/0,
         global_timestamp_binary/1]).


%% TODO: Incomplete!  100% of public API is not yet covered.

-spec(global_timestamp_binary/0 :: () -> binary()).
-spec(gregs_to_gentime/1 :: (non_neg_integer()) -> binary()).
-spec(time_t/0 :: () -> integer()).



%% @spec () -> integer()
%% @doc Return the current time as a <tt>time_t</tt>-style integer.

time_t() ->
    time_t(erlang:now()).

%% @spec ({integer(), integer(), integer()}) -> integer()
%% @doc Convert an Erlang-style <tt>now()</tt> timestamp as a
%%      <tt>time_t</tt>-style integer.

time_t({MSec, Sec, _}) ->
    (MSec * 1000000) + Sec.

%% @spec () -> {integer(), integer()}
%% @doc Return the current time as a <tt>time_t</tt>-style integer
%% plus milliseconds.

time_t_msec() ->
    time_t_msec(erlang:now()).

%% @spec ({integer(), integer(), integer()}) -> {integer(), integer()}
%% @doc Convert an Erlang-style <tt>now()</tt> timestamp as a
%%      <tt>time_t</tt>-style integer plus milliseconds.

time_t_msec({MSec, Sec, USec}) ->
    {(MSec * 1000000) + Sec, USec div 1000}.

%% @spec () -> {integer(), integer()}
%% @doc Return the current time as a <tt>time_t</tt>-style integer plus microseconds.

time_t_usec() ->
    time_t_usec(erlang:now()).

%% @spec ({integer(), integer(), integer()}) -> {integer(), integer()}
%% @doc Convert an Erlang-style <tt>now()</tt> timestamp as a
%%      <tt>time_t</tt>-style integer plus microseconds.

time_t_usec({MSec, Sec, USec}) ->
    {(MSec * 1000000) + Sec, USec}.

%% @spec () -> integer()
%% @doc Create a timestamp based on the current time (erlang:now()).

time_integer() ->
    time_integer(erlang:now()).

%% @spec ({integer(), integer(), integer()}) -> integer()
%% @doc Create a timestamp based on the current time (erlang:now()).

time_integer({MSec, Sec, USec}) ->
    (MSec * 1000000 * 1000000) + (Sec * 1000000) + USec.

%% @spec () -> {integer(),node()}
%% @doc construct a global time integer

global_time_integer() ->
    {time_integer(),erlang:node()}.

%% @spec ({integer(), integer(), integer()}) -> {integer(),node()}
%% @doc construct a global time integer

global_time_integer(Now) ->
    {time_integer(Now),erlang:node()}.

%% @spec (integer(), cal_time()) -> {cal_date(), cal_time()}
%% @doc Calculate the date of the immediate future's Day of the Week and time.

next_dow_time(DoW, Time) ->
    next_dow_time(DoW, Time, date(), time()).

%% @spec (integer(), cal_time(), cal_date(), cal_time()) -> cal_date()
%% @doc Calculate the date of the immediate future's Day of the Week and time,
%% relative to a given date and time.

next_dow_time(DoW, Time, NowDate, NowTime) ->
    NowDoW = calendar:day_of_the_week(NowDate),
    BaseDaysAheadOfNow =
        if DoW =:= NowDoW ->
                DesiredSecs = calendar:time_to_seconds(Time),
                NowSecs = calendar:time_to_seconds(NowTime),
                if NowSecs < DesiredSecs -> 0;
                   true                  -> 7
                end;
           DoW > NowDoW ->
                DoW - NowDoW;
           true ->
                7 - NowDoW + DoW
        end,
    %% Example: Now is a Wedesday afternoon: {2007,2,7}, {18,5,38}
    %% BaseDaysAheadofNow should be:
    %% > [gmt_time:next_dow_time_partial(I, {8,0,0}, {2007,2,7}, {18,5,38}) ||
    %%    I <- lists:seq(1,7)].
    %% [5,6,7,1,2,3,4]
    %% > [gmt_time:next_dow_time_partial(I, {23,0,0}, {2007,2,7}, {18,5,38}) ||
    %%    I <- lists:seq(1,7)].
    %% [5,6,0,1,2,3,4]
    %%     > [gmt_time:next_dow_time(I, {23,0,0}, {2007,2,7}, {18,5,38}) || I <- lists:seq(1,7)].
    %%     [{2007,2,12},
    %%      {2007,2,13},
    %%      {2007,2,7},  % 23:00 is in the future later today
    %%      {2007,2,8},
    %%      {2007,2,9},
    %%      {2007,2,10},
    %%      {2007,2,11}]
    %%     > [gmt_time:next_dow_time(I, {8,0,0}, {2007,2,7}, {18,5,38}) || I <- lists:seq(1,7)].
    %%     [{2007,2,12},
    %%      {2007,2,13},
    %%      {2007,2,14}, % 08:00 has passed for today, the next one is on the 14th
    %%      {2007,2,8},
    %%      {2007,2,9},
    %%      {2007,2,10},
    %%      {2007,2,11}]

    BaseDaysSeconds = BaseDaysAheadOfNow * (3600*24),
    BaseSeconds = calendar:time_to_seconds(Time),
    GregsNow = calendar:datetime_to_gregorian_seconds({NowDate, {0,0,0}}),
    GregsThen = GregsNow + BaseDaysSeconds + BaseSeconds,
    {ThenDate, _ThenTime} = calendar:gregorian_seconds_to_datetime(GregsThen),
    ThenDate.

%% @spec (list() | binary()) -> integer() | bad_date
%% @doc converts an rfc2616 date string to UNIX time_t format.

rfc2616_to_time_t(X) when is_list(X) ->
    %% blindly ignore all crashes and treat as bad_date
    case catch (httpd_util:convert_request_date(X)) of
        {{YYYY,MM,DD},{H,M,S}}=Y when YYYY >= 1970, YYYY =< 2037 ->
            case calendar:valid_date(YYYY,MM,DD) of
                true ->
                    case valid_time(H,M,S) of
                        true ->
                            calendar:datetime_to_gregorian_seconds(Y)
                                - 62167219200; %% Unix Epoch
                        false ->
                            bad_date
                    end;
                false ->
                    bad_date
            end;
        _ ->
            bad_date
    end;
rfc2616_to_time_t(X) when is_binary(X) ->
    rfc2616_to_time_t(binary_to_list(X)).

%% If a two digit year is encountered whose value is between 00 and 49,
%% the year is interpreted by adding 2000, ending up with a value
%% between 2000 and 2049. If a two digit year is encountered with a
%% value between 50 and 99, or any three digit year is encountered,
%% the year is interpreted by adding 1900

fix_year_rfc2822(X) when is_list(X) ->
    Year = list_to_integer(X),
    Len  = length(X),

    if
        Year < 0 ->
            bad_date;
        Len =:= 2 ->
            if
                Year >= 0 andalso Year =< 49 ->
                    Year + 2000;
                true ->
                    Year + 1900
            end;
        Len =:= 3 ->
            Year + 1900;
        true ->
            Year
    end.

%% If one digit date is given, e.g., "Wed, 9 Feb 2003 13:37:59 -0700"
%% change it to two digit format, e.g., "Wed, 09 Feb 2003 13:37:59 -0700"

fix_onedigit_date(X) when is_list(X) ->
    Len = length(X),

    if
        Len =:= 1 ->
            "0" ++ X;
        true ->
            X
    end.

%% If the seconds part is missing, treat it as :00
%% Customization: Fix single digit hour in BZ 27978
fix_second_rfc2822(X) when is_list(X) ->
    case string:tokens(X, ":") of
        [HH, _MM, _SS] ->
            Hour = list_to_integer(HH),
            Len = length(HH),
            if
                Hour < 10 andalso Len =:= 1 ->
                    "0" ++ X;
                true ->
                    X
            end;
        [HH, MM] ->
            lists:flatten(io_lib:format("~s:~s:00", [HH, MM]));
        _ ->
            bad_date
    end.

%% If the timzone is of ("+"/"-") XX:XX format, then take the colon out
%% If the timzone is of ("+"/"-") XX-XX format, then take the second dash out
fix_4digit_tz_rfc2822(X) ->
    Length = string:len(X),
    if
        Length < 5 ->
            bad_date;
        true ->
            Leading = string:substr(X,1,1),
            Y = string:substr(X,2),

            case string:tokens(Y, ":-") of
                [AA, BB] ->
                    Leading ++ AA ++ BB;
                _ ->
                    X
            end
    end.

is_valid_date(X) when is_list(X) ->
    DateList = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"],
    lists:member(X, DateList).

%% @spec (list() | binary()) -> integer() | bad_date
%% @doc converts an rfc2822 date string to UNIX time_t format.

rfc2822_to_time_t(X) when is_list(X) ->
    try
        case string:tokens(X, ",() ") of
            [Date, DD, MM, YYYY, HHMMSS, Tz_4digit, Tz_symbol] ->
                %% Sample: "Wed, 19 Feb [[2]0]03 13:37[:59] -0700 PDT"

                Day  = fix_onedigit_date(DD),
                Year = fix_year_rfc2822(YYYY),
                Time = fix_second_rfc2822(HHMMSS),

                Y = lists:flatten(
                       io_lib:format("~s, ~s ~3.s ~4.4.0w ~s",
                          [Date,Day,MM,Year,Time])),

                rfc2616_to_time_t(Y) - utc_offset_rfc2822(Tz_4digit, Tz_symbol);

            [X1, X2, X3, X4, X5, X6] ->
                %% can be either "Wed, 19 Feb [[2]0]03 13:37[:59] -0700/PDT"
                %%            or "     19 Feb [[2]0]03 13:37[:59] -0700 PDT"

                case is_valid_date(X1) of
                    true ->
                        Day  = fix_onedigit_date(X2),
                        Year = fix_year_rfc2822(X4),
                        Time = fix_second_rfc2822(X5),

                        Y = lists:flatten(
                               io_lib:format("~s, ~s ~3.s ~4.4.0w ~s",
                                  [X1,Day,X3,Year,Time])),

                        rfc2616_to_time_t(Y) - utc_offset_rfc2822(X6, "undefined");
                    false ->
                        Day  = fix_onedigit_date(X1),
                        Year = fix_year_rfc2822(X3),
                        Time = fix_second_rfc2822(X4),

                        Y = lists:flatten(
                               io_lib:format("XXX, ~s ~3.s ~4.4.0w ~s",
                                  [Day,X2,Year,Time])),

                        rfc2616_to_time_t(Y) - utc_offset_rfc2822(X5, X6)
                end;

            [DD, MM, YYYY, HHMMSS, Tz] ->
                %% Sample: "19 Feb [[2]0]03 13:37[:59] -0700/PDT"

                Day  = fix_onedigit_date(DD),
                Year = fix_year_rfc2822(YYYY),
                Time = fix_second_rfc2822(HHMMSS),

                Y = lists:flatten(
                       io_lib:format("XXX, ~s ~3.s ~4.4.0w ~s",
                          [Day,MM,Year,Time])),

                rfc2616_to_time_t(Y) - utc_offset_rfc2822(Tz, "undefined");
            _ ->
                bad_date
        end
    catch
        _AnyClass:_Any ->
            bad_date
    end;

rfc2822_to_time_t(X) when is_binary(X) ->
    rfc2822_to_time_t(binary_to_list(X)).

%% Tz input can be any one of the followings:
%%    -08[:]00       (things inside brackets are optional)
%%    PST
%%    -08[:]00 PST   (both 4DIGIT and symbol are provided)
%%
%% For now only trust the 4DIGIT value if both 4DIGIT and symbol are provided

utc_offset_rfc2822(Tz, _Tz_extra) ->
    Sign = string:left(Tz, 1, $.),
    if
        Sign =/= "+" andalso Sign =/= "-" ->
            %% not a Tz symbol, so look up its 4DIGIT value
            TzInt = valid_timezone(Tz);
        true ->
            %% is a 4DIGIT value, then fix the customized pattern if any
            TzInt = fix_4digit_tz_rfc2822(Tz)
    end,

    Input_tz = round(list_to_integer(TzInt) div 100) + ((list_to_integer(TzInt)) rem 100)/60,
    case Input_tz of
        0.0 ->
            Offset = 0;
        _ ->
            Offset = 3600*Input_tz
    end,
    round(Offset).

valid_timezone(TzStr) ->
    ZoneList   = [{"A", "-0000"},
                  {"a", "-0000"},
                  {"ACDT", "+1030"},
                  {"ACST", "+0930"},
                  {"ADT", "-0300"},
                  {"AEDT", "+1100"},
                  {"AEST", "+1000"},
                  {"AFT", "+0430"},
                  {"AHST", "-1000"},
                  {"AKDT", "-0800"},
                  {"AKST", "-0900"},
                  {"ALMT", "+0600"},
                  {"AQTT", "+0500"},
                  {"ART", "-0300"},
                  {"AST", "-0400"},
                  {"AT", "-0100"},
                  {"AWST", "+0800"},
                  {"AZOT", "-0100"},
                  {"B", "-0000"},
                  {"b", "-0000"},
                  {"BAT", "+0300"},
                  {"BDT", "+0600"},
                  {"BEST", "-1100"},
                  {"BNT", "+0800"},
                  {"BOT", "-0400"},
                  {"BORT", "+0800"},
                  {"BST", "+0100"},
                  {"BT", "+0300"},
                  {"BZT2", "-0300"},
                  {"C", "-0000"},
                  {"c", "-0000"},
                  {"CAT", "+0200"},
                  {"CAST", "+0930"},
                  {"CCT", "+0800"},
                  %%{"CDT","+1030"},  Australia Central Daylight
                  {"CDT", "-0500"},   %% North America Central Daylight
                  {"CEST", "+0200"},
                  {"CET", "+0100"},
                  {"CHAST", "+1245"},
                  {"CKT", "-1000"},
                  {"CLT", "-0400"},
                  {"COT", "-0500"},
                  %%{"CST","+0930"},  Australia
                  {"CST", "-0600"},   %% North America.
                  {"CXT", "+0700"},
                  {"CUT", "+0000"},
                  {"CVT", "-0100"},
                  {"D", "-0000"},
                  {"d", "-0000"},
                  {"DAVT", "+0700"},
                  {"DDUT", "+1000"},
                  {"DNT", "+0100"},
                  {"E", "-0000"},
                  {"e", "-0000"},
                  {"EAST", "-0600"},
                  {"EAT", "+0300"},
                  {"ECT", "-0500"},   %% Australia
                  {"EDT", "+1100"},   %% Australia
                  {"EDT", "-0400"},   %% North America
                  {"EEST", "+0300"},
                  {"EET", "+0200"},
                  %%{"EST","+1000"},  Australia
                  {"EST", "-0500"},   %% North America
                  {"EGT", "-0100"},
                  {"F", "-0000"},
                  {"f", "-0000"},
                  {"FJT", "+1200"},
                  {"FKT", "-0400"},
                  {"FST", "-0200"},
                  {"FWT", "+0100"},
                  {"G", "-0000"},
                  {"g", "-0000"},
                  {"GALT", "-0600"},
                  {"GAMT", "-0900"},
                  {"GFT", "-0300"},
                  {"GILT", "+1200"},
                  {"GMT", "+0000"},
                  {"GST", "+1000"},
                  {"BYT", "-0400"},
                  {"GZ", "+0000"},
                  {"H", "-0000"},
                  {"h", "-0000"},
                  {"HAA", "-0300"},
                  {"HAC", "-0500"},
                  {"HADT", "-0900"},
                  {"HAE", "-0400"},
                  {"HAP", "-0700"},
                  {"HAR", "-0600"},
                  {"HAST", "-1000"},
                  {"HAT", "-0230"},
                  {"HAY", "-0800"},
                  {"HFE", "+0200"},
                  {"HFH", "+0100"},
                  {"HG", "+0000"},
                  {"HKT", "+0800"},
                  {"HNA", "-0400"},
                  {"HNC", "-0600"},
                  {"HNE", "-0500"},
                  {"HNP", "-0800"},
                  {"HNR", "-0700"},
                  {"HNT", "-0330"},
                  {"HNY", "-0900"},
                  {"HOE", "+0100"},
                  {"HST", "-1000"},
                  {"I", "-0000"},
                  {"i", "-0000"},
                  {"ICT", "+0700"},
                  {"IDLE", "+1200"},
                  {"IDLW", "-1200"},
                  {"IOT", "+0500"},
                  {"IST", "+0100"},   %% Irish standard
                  %%{"IST","+0530"},  India calcutta
                  {"IRT", "+0330"},   %% Irish standard
                  {"IT", "+0330"},    %% Irish standard
                  {"JST", "+0900"},
                  {"JT", "+0700"},
                  {"JAVT", "+0900"},
                  {"K", "-0000"},
                  {"k", "-0000"},
                  {"KGT", "+0500"},
                  {"KST", "+0900"},
                  {"KOST", "+1200"},
                  {"KRAT", "+0700"},
                  {"L", "-0000"},
                  {"l", "-0000"},
                  {"LHST", "+1000"},
                  {"LKT", "+0600"},
                  {"LINT", "+1400"},
                  {"M", "-0000"},
                  {"m", "-0000"},
                  {"MAGT", "+1130"},
                  {"MAWT", "+0600"},
                  {"MDT", "-0600"},
                  {"MESZ", "+0200"},
                  {"MET", "+0100"},
                  {"MEZ", "-0600"},
                  {"MEZ", "+0100"},
                  {"MHT", "+1200"},
                  {"MMT", "+0630"},
                  {"MPT", "+1000"},
                  {"MSK", "+0300"},
                  {"MST", "-0700"},
                  {"MUT", "+0400"},
                  {"MVT", "+0500"},
                  {"MYT", "+0800"},
                  {"N", "-0000"},
                  {"n", "-0000"},
                  {"NCT", "+1130"},
                  {"NDT", "-0230"},
                  {"NFT", "+1130"},
                  {"NOVT", "+0600"},
                  {"NOR", "+0100"},
                  {"NPT", "+0545"},
                  {"NRT", "+1200"},
                  {"NST", "-0330"},
                  {"NSUT", "+0630"},
                  {"NUT", "-1100"},
                  {"NZT", "+1200"},
                  {"O", "-0000"},
                  {"o", "-0000"},
                  {"OET", "+0200"},
                  {"OMST", "+0600"},
                  {"P", "-0000"},
                  {"p", "-0000"},
                  {"PDT", "-0700"},
                  {"PET", "-0500"},
                  {"PETT", "+1200"},
                  {"PGT", "+1000"},
                  {"PHOT", "+1300"},
                  {"PHT", "+0800"},
                  {"PKT", "+0500"},
                  {"PMT", "-0300"},
                  {"PONT", "+1130"},
                  {"PST", "-0800"},
                  {"PWT", "+0900"},
                  {"PYT", "-0400"},
                  {"Q", "-0000"},
                  {"q", "-0000"},
                  {"R", "-0000"},
                  {"r", "-0000"},
                  {"RET", "+0400"},
                  {"R2T", "+0300"},
                  {"S", "-0000"},
                  {"s", "-0000"},
                  {"SAST", "+0930"},
                  {"SBT", "+1130"},
                  {"SCT", "+0400"},
                  {"SET", "+0100"},
                  {"SRT", "-0300"},
                  {"SST", "+0800"},
                  {"SWT", "+0100"},
                  {"T", "-0000"},
                  {"t", "-0000"},
                  {"THA", "+0700"},
                  {"THAT", "-1000"},
                  {"TFT", "+0500"},
                  {"TJT", "+0500"},
                  {"TKT", "-1000"},
                  {"TMT", "+0500"},
                  {"TOT", "+1300"},
                  {"TST", "+0300"},
                  {"TRUT", "+1000"},
                  {"TVT", "+1200"},
                  {"TUC", "+0000"},
                  {"U", "-0000"},
                  {"u", "-0000"},
                  {"ULAT", "+0800"},
                  {"USZ1", "+0200"},
                  {"USZ2", "+0300"},
                  {"USZ3", "+0400"},
                  {"USZ4", "+0500"},
                  {"USZ5", "+0600"},
                  {"USZ6", "+0700"},
                  {"USZ7", "+0800"},
                  {"USZ8", "+0900"},
                  {"USZ9", "+1000"},
                  {"UT",  "+0000"},
                  {"UTC", "+0000"},
                  {"UTZ", "-0300"},
                  {"UYT", "-0300"},
                  {"UZT", "+0500"},
                  {"UZ10", "+1130"},
                  {"UZ11", "+1200"},
                  {"V", "-0000"},
                  {"v", "-0000"},
                  {"VET", "-0400"},
                  {"VLAT", "+1000"},
                  {"VTZ", "-0200"},
                  {"VUT", "+1130"},
                  {"W", "-0000"},
                  {"w", "-0000"},
                  {"WAT", "+0100"},
                  {"WAKT", "+1200"},
                  {"WEST", "+0100"},
                  {"WET", "+0000"},
                  {"WEZ", "+0000"},
                  {"WFT", "+1200"},
                  {"WGT", "-0300"},
                  {"WST", "+0800"},
                  {"WUT", "+0100"},
                  {"WZ", "+0000"},
                  {"X", "-0000"},
                  {"x", "-0000"},
                  {"Y", "-0000"},
                  {"y", "-0000"},
                  {"YAKT", "+0900"},
                  {"YAPT", "+1000"},
                  {"YEKT", "+0500"},
                  {"YST", "-0900"},
                  {"Z", "-0000"},
                  {"z", "-0000"}],
    case lists:keyfind(TzStr, 1, ZoneList) of
        {TzStr, Int} ->
            Int;
        _   ->
            bad_date
    end.

valid_time(H,M,S)
  when H >= 0, H =< 23, M >= 0, M =< 59, S >= 0, S =< 59 ->
    true;
valid_time(_H,_M,_S) ->
    false.

%% @spec () -> list()
%% @doc converts an UNIX time_t now to rfc1123 format.

time_t_to_rfc1123() ->
    time_t_to_rfc1123(time_t()).

%% @spec (integer()) -> list()
%% @doc converts an UNIX time_t to rfc1123 format.

time_t_to_rfc1123(X) when is_integer(X), X >= 0 ->
    Y = calendar:now_to_local_time({X div 1000000, X rem 1000000, 0}),
    httpd_util:rfc1123_date(Y).

%% @spec (integer()) -> binary()
%% @doc converts an UNIX time_t to rfc1123 format binary.

time_t_to_rfc1123_binary(X) ->
    list_to_binary(time_t_to_rfc1123(X)).

%% @spec () -> binary()
%% @doc converts an UNIX time_t now to rfc1123 format binary.

time_t_to_rfc1123_binary() ->
    time_t_to_rfc1123_binary(time_t()).

%% @spec () -> list()
%% @doc converts an UNIX time_t now to rfc2822 format.

time_t_to_rfc2822() ->
    time_t_to_rfc2822(time_t()).

%% @spec (integer()) -> list()
%% @doc converts an UNIX time_t to rfc2822 format.

time_t_to_rfc2822(X) when is_integer(X), X >= 0 ->
    Y = calendar:now_to_local_time({X div 1000000, X rem 1000000, 0}),
    rfc2822_date( Y ).

rfc2822_date( LocalTime ) ->

    case LocalTime of
        {{YYYY,MM,DD},{Hour,Min,Sec}} when YYYY >= 1970, YYYY =< 2037 ->

            %% get GMT Time
            case calendar:local_time_to_universal_time_dst(LocalTime) of
                [Gmt]   -> Gmt;
                [_,Gmt] -> Gmt
            end,

            %% calculate timezone
            Gtime_sec = calendar:datetime_to_gregorian_seconds(Gmt),
            Ltime_sec = calendar:datetime_to_gregorian_seconds(LocalTime),

            Time_diff = round( (Gtime_sec - Ltime_sec) / 3600),

            if
                Time_diff > 0 ->
                    Tz = io_lib:format("-~2.2.0w00", [abs(Time_diff)]);
                Time_diff =< 0 ->
                    Tz = io_lib:format("+~2.2.0w00", [abs(Time_diff)])
            end,

            DayNumber = calendar:day_of_the_week({YYYY,MM,DD}),
            lists:flatten(
              io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w ~s",
                            [httpd_util:day(DayNumber),DD,httpd_util:month(MM),YYYY,Hour,Min,Sec,Tz]));
        _ ->
            bad_date
    end.

%% @spec () -> integer()
%% @doc Return the number of seconds of current time zone offset.

utc_offset() ->
    case string:strip(os:cmd("date +%z"), right, 10) of
        "" ->
            0; %% "date" doesn't know....
        L ->
            Sign = case hd(L) of $+ -> 1;
                       $- -> -1 end,
            [_, Ht, Ho, Mt, Mo] = L,
            Sign * (((Ht-$0) * 10) + (Ho-$0))*3600 +
                (((Mt-$0) * 10) + (Mo-$0))*60
    end.

%% @spec (T::generalized_time()) -> integer()
%% @doc Convert an ASN.1 "GeneralizedTime" string to Gregorian seconds.
%% Note that Gregorian seconds are <b>not</b> the same as time_t seconds!

gentime_to_gregs(T) when is_list(T) ->
    gentime_to_gregs(list_to_binary(T));
gentime_to_gregs(<<YYYY:4/binary, MMo:2/binary, DD:2/binary,
                  HH:2/binary, MMi:2/binary, SS:2/binary,
                  X/binary>> = B)
  when byte_size(B) =:= 17, X =:= <<".0Z">> ->
    gentime_to_gregs2(YYYY, MMo, DD, HH, MMi, SS);
gentime_to_gregs(<<YYYY:4/binary, MMo:2/binary, DD:2/binary,
                  HH:2/binary, MMi:2/binary, SS:2/binary,
                  X/binary>> = B)
  when byte_size(B) =:= 15, X =:= <<"Z">> ->
    gentime_to_gregs2(YYYY, MMo, DD, HH, MMi, SS).

gentime_to_gregs2(YYYY, MMo, DD, HH, MMi, SS) ->
    Year = list_to_integer(binary_to_list(YYYY)),
    Mon = list_to_integer(binary_to_list(MMo)),
    Day = list_to_integer(binary_to_list(DD)),
    Hour = list_to_integer(binary_to_list(HH)),
    Min = list_to_integer(binary_to_list(MMi)),
    Sec = list_to_integer(binary_to_list(SS)),
    T = {{Year, Mon, Day}, {Hour, Min, Sec}},
    calendar:datetime_to_gregorian_seconds(T).

%% @spec (S::integer()) -> generalized_time()
%% @doc Convert Gregorian seconds to an ASN.1 "GeneralizedTime" string.

gregs_to_gentime(S) when is_integer(S) ->
    CalDate = calendar:gregorian_seconds_to_datetime(S),
    list_to_binary([cal_to_bignumstr(CalDate), ".0Z"]).

gregs_to_gentime2(S) when is_integer(S) ->
    {{Year, Mon, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(S),
    NewCalDate = {{Year+1970,Mon,Day},{Hour,Min,Sec}},
    list_to_binary([cal_to_bignumstr(NewCalDate), ".0Z"]).

%% @spec (calendar_date()) -> integer()
%% @doc Convert calendar module-style date &amp; time to a bignum.
%% The format of the bignum's digits is: YYYYMMDDHHMMSS.

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
    gmt_util:left_pad(integer_to_list(N), 14, $0).


smpp_to_abs(Time) when is_list(Time) ->
    smpp_to_abs(list_to_binary(Time));
smpp_to_abs(<<T:16/binary, _N:1/binary>> = Time) when is_binary(Time) ->
    smpp_to_abs(T);
smpp_to_abs(<<YY:2/binary, MM:2/binary, DD:2/binary, HH:2/binary, Mm:2/binary, SS:2/binary, TS:1/binary, QD:2/binary, P:1/binary>> = B) when is_binary(B) ->
    Year = list_to_integer(binary_to_list(YY)),
    Month = list_to_integer(binary_to_list(MM)),
    Day = list_to_integer(binary_to_list(DD)),
    Hour = list_to_integer(binary_to_list(HH)),
    Min = list_to_integer(binary_to_list(Mm)),
    Sec = list_to_integer(binary_to_list(SS)),
    _TSec = list_to_integer(binary_to_list(TS)),
    QDiff = list_to_integer(binary_to_list(QD)),

    case P of
        <<"+">> ->
            DT = {{(2000 + Year), Month, Day}, {Hour, Min, Sec}},
            TT = calendar:datetime_to_gregorian_seconds(DT),
            (TT + (QDiff * 15 * 60));
        <<"-">> ->
            DT = {{(2000 + Year), Month, Day}, {Hour, Min, Sec}},
            TT = calendar:datetime_to_gregorian_seconds(DT),
            (TT - (QDiff * 15 * 60));
        <<"R">> ->
            DT = {{Year, Month, Day}, {Hour, Min, Sec}},
            TT = calendar:datetime_to_gregorian_seconds(DT),
            (TT + time_t());
        Unknown ->
            {error, io_lib:format("gmt_time:smpp_to_abs: unknown orientation flag ~p", [Unknown])}
    end.

%% @spec (infinity | integer() | {integer(), integer(), integer()}) -> {integer(), integer(), integer()}
%% @doc Construct an Erlang-style timestamp given an expiry in
%% milliseconds relative to erlang:now().  Treat an expiry having a
%% MSecs of zero as a symbolic deadline rather than an absolute
%% timestamp.

make_expires(infinity) ->
    make_expires(erlang:now(), infinity);
make_expires(Timeout) when is_integer(Timeout) ->
    make_expires(erlang:now(), Timeout);
make_expires({0, _S, _US}=_Expires) ->
    {MS, S, US} = erlang:now(),
    {MS, S + _S, US + _US};
make_expires({_MS, _S, _US}=Expires) ->
    Expires.

%% @spec ({integer(), integer(), integer()}, integer()) -> {integer(), integer(), integer()}
%% @doc Construct an Erlang-style timestamp given an expiry in
%% milliseconds relative to given value.  Treat an expiry having a
%% MSecs of zero as a symbolic deadline rather than an absolute
%% timestamp.

make_expires({MS, S, US}, infinity) ->
    {MS + 987654321, S, US}; % Just make a really big number.
make_expires({MS, S, US}, Timeout) when is_integer(Timeout) ->
    {MS, S, US + (Timeout * 1000)};
make_expires({MS, S, US}, {0, _S, _US}) ->
    {MS, S + _S, US + _US}.

%% @spec (float(), {integer(), integer(), integer()}) -> {integer(),
%% integer(), integer()}
%% @doc Multiply an Erlang-style timestamp by the given factor.

multiply_expires(Factor, Expires) ->
    multiply_expires(Factor, Expires, erlang:now()).

%% @spec (float(), {integer(), integer(), integer()}, {integer(),
%% integer(), integer()}) -> {integer(), integer(), integer()}
%% @doc Multiply an Erlang-style timestamp by the given factor
%% relative to given value of Now.

multiply_expires(Factor, {_MS, _S, _US}, _Now) when Factor =< 0 ->
    {0, 0, 0}; %% NOTE: caution w/ symbolic deadline timestamps
multiply_expires(Factor, Expires, Now) ->
    case make_timeout(Now, Expires) of
        infinity ->
            Expires;
        Timeout ->
            make_expires(Now, erlang:trunc(Timeout * Factor))
    end.

%% @spec (infinity | integer() | {integer(), integer(), integer()}) -> integer() | infinity
%% @doc Construct an Erlang-style timeout given an Erlang-style
%% timestamp.  Treat a timestamp having a MSecs of zero as a symbolic
%% deadline rather than an absolute timestamp.

make_timeout(infinity) ->
    infinity;
make_timeout(Timeout) when is_integer(Timeout) ->
    Timeout;
make_timeout({0, Sec, USec}) ->
    Y = trunc(((Sec * 1000000) + USec)/1000),
    if Y > 16#ffffffff -> % Just treat as infinity.
            infinity;
       true ->
            Y
    end;
make_timeout(Expires) ->
    make_timeout(erlang:now(), Expires).

%% @spec ({integer(), integer(), integer()}, {integer(), integer(), integer()}) -> integer() | infinity

%% @doc Construct an Erlang-style timeout given an Erlang-style
%% timestamp relative to given value of Now.  Treat a timestamp having
%% a MSecs of zero as a symbolic deadline rather than an absolute
%% timestamp.

make_timeout(_, infinity) ->
    infinity;
make_timeout(_Now, Timeout) when is_integer(Timeout) ->
    Timeout;
make_timeout(_Now, {0, Sec, USec}) ->
    Y = trunc(((Sec * 1000000) + USec)/1000),
    if Y > 16#ffffffff -> % Just treat as infinity.
            infinity;
       true ->
            Y
    end;
make_timeout(Now, Expires) ->
    X = timer:now_diff(Expires, Now),
    if X > 0 ->
            Y = trunc(X/1000),
            if Y > 16#ffffffff -> % Just treat as infinity.
                    infinity;
               true ->
                    Y
            end;
       true ->
            0
    end.

%% @spec (timeout()) -> timeout()
%% @doc Return the timer value (while checking for negative and
%% infinity values).  Input value is in the units of seconds or the
%% atom infinity

sec_timeout(Timeout) ->
    if Timeout =/= infinity ->
            if Timeout > 0 ->
                    timer:seconds(Timeout);
               true ->
                    0
            end;
       true ->
            infinity
    end.

%% @spec (timeout(), timeout()) -> timeout()
%% @doc Return the mininum timer value (while checking for negative
%% and infinity values).  Input values are in the units of seconds or
%% the atom infinity

minsec_timeout(Timeout1, Timeout2) ->
    case min_timeout(Timeout1, Timeout2) of
        infinity ->
            infinity;
        0 ->
            0;
        Timeout ->
            timer:seconds(Timeout)
    end.

%% @spec (timeout(), timeout()) -> timeout()
%% @doc Return the mininum timer value (while checking for negative
%% and infinity values).  Input values are in the units of milliseconds
%% or the atom infinity

min_timeout(Timeout1, Timeout2) ->
    if Timeout1 =/= infinity ->
            if Timeout1 > 0 ->
                    if Timeout2 =/= infinity ->
                            if Timeout2 > 0 ->
                                    lists:min([Timeout1, Timeout2]);
                               Timeout2 =< 0 ->
                                    0;
                               true ->
                                    Timeout1
                            end;
                       true ->
                            Timeout1
                    end;
               Timeout1 =< 0 ->
                    0;
               true ->
                    if Timeout2 =/= infinity ->
                            if Timeout2 > 0 ->
                                    Timeout2;
                               true ->
                                    0
                            end;
                       true ->
                            infinity
                    end
            end;
       true ->
            if Timeout2 =/= infinity ->
                    if Timeout2 > 0 ->
                            Timeout2;
                       true ->
                            0
                    end;
               true ->
                    infinity
            end
    end.

%% @spec (timeout(), timeout()) -> timeout()
%% @doc Add 2 timeout values.  Input values are in the units of
%% milliseconds or the atom infinity

add_timeouts(Timeout1, Timeout2)
  when Timeout1 =:= infinity orelse Timeout2 =:= infinity ->
    infinity;

add_timeouts(Timeout1, Timeout2) ->
    Timeout1  + Timeout2.

%% @spec () -> {term(),node()}
%% @doc construct a global timestamp
global_timestamp() ->
    {erlang:now(),erlang:node()}.

%% @spec (Now::term()) -> {term(),node()}
%% @doc construct a global timestamp
global_timestamp(Now) ->
    {Now,erlang:node()}.

%% @spec ({Now2::term(), Node2::node()}, {Now1::term(), Node1::node()}) -> 0 | -1 | 1
%% @doc compare two global timestamps
global_timestamp_cmp({Now2, Node2}, {Now1, Node1}) ->
    Diff = timer:now_diff(Now2, Now1),
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

%% @spec (Ts2::term(), Ts1::term()) -> true | false
%% @doc less two global timestamps
global_timestamp_less(Ts2, Ts1) ->
    global_timestamp_cmp(Ts2, Ts1) =:= -1.

%% @spec () -> binary()
%% @doc construct a global timestamp and encode as binary with no
%% whitespaces
global_timestamp_binary() ->
    global_timestamp_binary(global_timestamp()).

%% @spec (Ts::term()) -> binary()
%% @doc encode a global timestamp as binary with no whitespaces
global_timestamp_binary(Ts) ->
    base64:encode(erlang:term_to_binary(Ts)).

%%%
%%% Misc edoc stuff
%%%

%% @type cal_date() = {integer(), integer(), integer()}
%% @type cal_time() = {integer(), integer(), integer()}
%% @type calendar_date() = {cal_date(), cal_time}
