-module(iso8601_tests).

-include_lib("eunit/include/eunit.hrl").

% midnight, as a calendar:time()
-define(MN, {0, 0, 0}).
% midnight, as a calendar:time(), with microseconds
-define(MNE, {0, 0, +0.0}).

parse_fail_test_() ->
    F = fun iso8601:parse/1,
    [{"fails to parse YYYYMM", ?_assertError(badarg, F("201212"))}].

parse_year_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYY", ?_assertMatch({{2012, 1, 1}, ?MN}, F("2012"))}].

parse_month_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYY-MM", ?_assertMatch({{2012, 12, 1}, ?MN}, F("2012-12"))}].

parse_month_day_test_() ->
    F = fun iso8601:parse/1,
    [
        {"parses YYYYMMDD", ?_assertMatch({{2012, 12, 12}, ?MN}, F("2012-12-12"))},
        {"parses YYYY-MM-DD", ?_assertMatch({{2012, 12, 12}, ?MN}, F("20121212"))}
    ].

parse_week_test_() ->
    F = fun iso8601:parse/1,
    [
        {"parses 2009W01 as 2008-12-29", ?_assertMatch({{2008, 12, 29}, ?MN}, F("2009W01"))},
        {"parses 2009-W01 as 2008-12-29", ?_assertMatch({{2008, 12, 29}, ?MN}, F("2009-W01"))},
        {"parses 2009W53 as 2010-01-03", ?_assertMatch({{2009, 12, 28}, ?MN}, F("2009W53"))},
        {"parses 2009-W53 as 2010-01-03", ?_assertMatch({{2009, 12, 28}, ?MN}, F("2009-W53"))}
    ].

parse_week_day_test_() ->
    F = fun iso8601:parse/1,
    [
        {"parses 2009W011 as 2008-12-29", ?_assertMatch({{2008, 12, 29}, ?MN}, F("2009W011"))},
        {"parses 2009-W01-1 as 2008-12-29", ?_assertMatch({{2008, 12, 29}, ?MN}, F("2009-W01-1"))},
        {"parses 2009W537 as 2010-01-03", ?_assertMatch({{2010, 1, 3}, ?MN}, F("2009W537"))},
        {"parses 2009-W53-7 as 2010-01-03", ?_assertMatch({{2010, 1, 3}, ?MN}, F("2009-W53-7"))}
    ].

parse_hour_test_() ->
    F = fun iso8601:parse/1,
    [
        {"parses YYYYMMDDTHH", ?_assertMatch({{2012, 2, 3}, {4, 0, 0}}, F("20120203T04"))},
        {"parses YYYY-MM-DDTHH", ?_assertMatch({{2012, 2, 3}, {4, 0, 0}}, F("2012-02-03T04"))}
    ].

parse_fractional_hour_test_() ->
    F = fun iso8601:parse/1,
    [
        {"parses YYYYMMDDTHH.hh", ?_assertMatch({{2012, 2, 3}, {4, 15, 0}}, F("20120203T04.25"))},
        {"parses YYYY-MM-DDTHH.hh",
            ?_assertMatch({{2012, 2, 3}, {4, 15, 0}}, F("2012-02-03T04.25"))},
        {"parses YYYYMMDDTHH,hh", ?_assertMatch({{2012, 2, 3}, {4, 15, 0}}, F("20120203T04,25"))},
        {"parses YYYY-MM-DDTHH,hh",
            ?_assertMatch({{2012, 2, 3}, {4, 15, 0}}, F("2012-02-03T04,25"))}
    ].

parse_minute_test_() ->
    F = fun iso8601:parse/1,
    [
        {"parses YYYYMMDDTHHMM", ?_assertMatch({{2012, 2, 3}, {4, 5, 0}}, F("20120203T0405"))},
        {"parses YYYY-MM-DDTHH:MM", ?_assertMatch({{2012, 2, 3}, {4, 5, 0}}, F("2012-02-03T04:05"))}
    ].

parse_fractional_minute_test_() ->
    F = fun iso8601:parse/1,
    [
        {"parses YYYYMMDDTHHMM.mm",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 15}}, F("20120203T0405.25"))},
        {"parses YYYY-MM-DDTHHMM.mm",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 15}}, F("2012-02-03T0405.25"))},
        {"parses YYYYMMDDTHHMM,mm",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 15}}, F("20120203T0405,25"))},
        {"parses YYYY-MM-DDTHHMM,mm",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 15}}, F("2012-02-03T0405,25"))}
    ].

parse_second_test_() ->
    F = fun iso8601:parse/1,
    [
        {"parses YYYYMMDDTHHMMSS", ?_assertMatch({{2012, 2, 3}, {4, 5, 6}}, F("20120203T040506"))},
        {"parses YYYY-MM-DDTHH:MM:SS",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 6}}, F("2012-02-03T04:05:06"))}
    ].

parse_fractional_second_test_() ->
    F = fun iso8601:parse/1,
    [
        {"parses YYYYMMDDTHHMMSS.ss",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 6}}, F("20120203T040506.50"))},
        {"parses YYYY-MM-DDTHHMMSS.ss",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 6}}, F("2012-02-03T040506.50"))},
        {"parses YYYYMMDDTHHMMSS,ss",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 6}}, F("20120203T040506,50"))},
        {"parses YYYY-MM-DDTHHMMSS,ss",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 6}}, F("2012-02-03T040506,50"))}
    ].

parse_exact_fractional_second_test_() ->
    F = fun iso8601:parse_exact/1,
    [
        {"parses YYYYMMDDTHHMMSS.ss",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 6.50}}, F("20120203T040506.50"))},
        {"parses YYYY-MM-DDTHHMMSS.ss",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 6.50}}, F("2012-02-03T040506.50"))},
        {"parses YYYYMMDDTHHMMSS,ss",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 6.50}}, F("20120203T040506,50"))},
        {"parses YYYY-MM-DDTHHMMSS,ss",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 6.50}}, F("2012-02-03T040506,50"))}
    ].

parse_fractional_fail_test_() ->
    F = fun iso8601:parse/1,
    % disallowed by spec
    [{"fails to parses multiple decimals", ?_assertError(badarg, F("20120203T04.25:05.25:06"))}].

parse_offset_test_() ->
    F = fun iso8601:parse/1,
    [
        {"parses YYYYMMDDTHHMMSS.ssZ",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 6}}, F("20120203T040506.50Z"))},
        {"parses YYYYMMDDTHHMMSS.ss+0400",
            ?_assertMatch({{2012, 2, 3}, {15, 9, 6}}, F("20120203T200506.50+0456"))},
        {"parses YYYYMMDDTHHMMSS.ss+0400",
            ?_assertMatch({{2012, 2, 3}, {17, 11, 6}}, F("20120203T040506.50-1306"))}
    ].

parse_duration_test_() ->
    F = fun iso8601:parse_duration/1,
    [
        {"parses with pos sign",
            ?_assertMatch(
                [
                    {sign, "+"},
                    {years, 6},
                    {months, 3},
                    {days, 1},
                    {hours, 1},
                    {minutes, 1},
                    {seconds, 1}
                ],
                F("+P6Y3M1DT1H1M1.1S")
            )},
        {"parses without sign",
            ?_assertMatch(
                [
                    {sign, []},
                    {years, 6},
                    {months, 3},
                    {days, 1},
                    {hours, 1},
                    {minutes, 1},
                    {seconds, 1}
                ],
                F("P6Y3M1DT1H1M1.1S")
            )},
        {"parses only years",
            ?_assertMatch(
                [
                    {sign, []},
                    {years, 6},
                    {months, 0},
                    {days, 0},
                    {hours, 0},
                    {minutes, 0},
                    {seconds, 0}
                ],
                F("P6Y")
            )},
        {"parses only minutes",
            ?_assertMatch(
                [
                    {sign, []},
                    {years, 0},
                    {months, 0},
                    {days, 0},
                    {hours, 0},
                    {minutes, 6},
                    {seconds, 0}
                ],
                F("PT6M")
            )}
    ].

parse_duration_fail_test_() ->
    F = fun iso8601:parse_duration/1,
    [{"fails to parses misspelled string", ?_assertError(badarg, F("PIY"))}].

parse_ordinal_test_() ->
    F = fun iso8601:parse/1,
    [
        {"parses YYYY-DDDTHHMMSS",
            ?_assertMatch({{2016, 2, 3}, {4, 5, 6}}, F("2016-034T040506.50"))},
        {"parses YYYY-DDD", ?_assertMatch({{2016, 2, 3}, ?MN}, F("2016-034"))},
        {"parses YYYY_DDDTHHMMSS, Feb 29 in a leap year",
            ?_assertMatch({{2016, 2, 29}, {4, 5, 6}}, F("2016-060T040506.50"))},
        {"parses YYYY_DDDTHHMMSS, after Feb 29 in a leap year",
            ?_assertMatch({{2016, 9, 25}, {4, 5, 6}}, F("2016-269T040506.50"))},
        {"parses YYYY_DDDTHHMMSS, Feb 29 in a leap century",
            ?_assertMatch({{2000, 2, 29}, {4, 5, 6}}, F("2000-060T040506.50"))},
        {"parses YYYY_DDDTHHMMSS, March 1 in a non-leap year",
            ?_assertMatch({{2015, 3, 1}, {4, 5, 6}}, F("2015-060T040506.50"))},
        {"parses YYYY_DDDTHHMMSS, after Feb 29 in a non-leap year",
            ?_assertMatch({{2015, 9, 26}, {4, 5, 6}}, F("2015-269T040506.50"))},
        {"parses YYYY_DDDTHHMMSS, March 1 in a non-leap century",
            ?_assertMatch({{1900, 3, 1}, {4, 5, 6}}, F("1900-060T040506.50"))},
        {"fails to parse ordinal date with 0 days", ?_assertError(badarg, F("2016-000T040506.50"))},
        {"fails to parse ordinal date with too many days in a leap year",
            ?_assertError(badarg, F("2016-367T040506.50"))},
        {"parses ordinal date with 366 days in a leap year",
            ?_assertMatch({{2016, 12, 31}, {4, 5, 6}}, F("2016-366T040506.50"))},
        {"fails to parse ordinal date with too many days in a non-leap year",
            ?_assertError(badarg, F("2015-366T040506.50"))}
    ].

parse_ordinal_exact_test_() ->
    F = fun iso8601:parse_exact/1,
    [
        {"parses YYYY-DDDTHHMMSS",
            ?_assertMatch({{2016, 2, 3}, {4, 5, 6.50}}, F("2016-034T040506.50"))},
        {"parses YYYY-DDD", ?_assertMatch({{2016, 2, 3}, ?MNE}, F("2016-034"))},
        {"parses YYYY_DDDTHHMMSS, Feb 29 in a leap year",
            ?_assertMatch({{2016, 2, 29}, {4, 5, 6.50}}, F("2016-060T040506.50"))},
        {"parses YYYY_DDDTHHMMSS, after Feb 29 in a leap year",
            ?_assertMatch({{2016, 9, 25}, {4, 5, 6.50}}, F("2016-269T040506.50"))},
        {"parses YYYY_DDDTHHMMSS, Feb 29 in a leap century",
            ?_assertMatch({{2000, 2, 29}, {4, 5, 6.50}}, F("2000-060T040506.50"))},
        {"parses YYYY_DDDTHHMMSS, March 1 in a non-leap year",
            ?_assertMatch({{2015, 3, 1}, {4, 5, 6.50}}, F("2015-060T040506.50"))},
        {"parses YYYY_DDDTHHMMSS, after Feb 29 in a non-leap year",
            ?_assertMatch({{2015, 9, 26}, {4, 5, 6.50}}, F("2015-269T040506.50"))},
        {"parses YYYY_DDDTHHMMSS, March 1 in a non-leap century",
            ?_assertMatch({{1900, 3, 1}, {4, 5, 6.50}}, F("1900-060T040506.50"))},
        {"fails to parse ordinal date with 0 days", ?_assertError(badarg, F("2016-000T040506.50"))},
        {"fails to parse ordinal date with too many days in a leap year",
            ?_assertError(badarg, F("2016-367T040506.50"))},
        {"parses ordinal date with 366 days in a leap year",
            ?_assertMatch({{2016, 12, 31}, {4, 5, 6.50}}, F("2016-366T040506.50"))},
        {"fails to parse ordinal date with too many days in a non-leap year",
            ?_assertError(badarg, F("2015-366T040506.50"))}
    ].

add_time_test_() ->
    F = fun iso8601:add_time/4,
    [
        {"add one second",
            ?_assertMatch({{2017, 11, 28}, {17, 7, 58}}, F({{2017, 11, 28}, {17, 7, 57}}, 0, 0, 1))},
        {"add one minute",
            ?_assertMatch({{2017, 11, 28}, {17, 8, 57}}, F({{2017, 11, 28}, {17, 7, 57}}, 0, 1, 0))},
        {"add one hour",
            ?_assertMatch({{2017, 11, 28}, {18, 7, 57}}, F({{2017, 11, 28}, {17, 7, 57}}, 1, 0, 0))},
        {"roll over to next day",
            ?_assertMatch(
                {{2017, 11, 29}, {00, 30, 00}}, F({{2017, 11, 28}, {23, 30, 00}}, 1, 0, 0)
            )}
    ].

subtract_time_test_() ->
    F = fun iso8601:subtract_time/4,
    [
        {"add one second",
            ?_assertMatch({{2017, 11, 28}, {17, 7, 56}}, F({{2017, 11, 28}, {17, 7, 57}}, 0, 0, 1))},
        {"add one minute",
            ?_assertMatch({{2017, 11, 28}, {17, 6, 57}}, F({{2017, 11, 28}, {17, 7, 57}}, 0, 1, 0))},
        {"add one hour",
            ?_assertMatch({{2017, 11, 28}, {16, 7, 57}}, F({{2017, 11, 28}, {17, 7, 57}}, 1, 0, 0))},
        {"roll back to previous day",
            ?_assertMatch(
                {{2017, 11, 28}, {23, 30, 00}}, F({{2017, 11, 29}, {00, 30, 00}}, 1, 0, 0)
            )}
    ].

add_months_test_() ->
    F = fun iso8601:add_months/2,
    [
        {"add one month in the middle of the year",
            ?_assertMatch({{2017, 6, 24}, {1, 2, 3}}, F({{2017, 5, 24}, {1, 2, 3}}, 1))},
        {"add one month at the beginning of the year",
            ?_assertMatch({{2017, 2, 24}, {1, 2, 3}}, F({{2017, 1, 24}, {1, 2, 3}}, 1))},
        {"add one month almost in the end of the year",
            ?_assertMatch({{2017, 12, 24}, {1, 2, 3}}, F({{2017, 11, 24}, {1, 2, 3}}, 1))},
        {"add one month in the end of the year",
            ?_assertMatch({{2018, 1, 24}, {1, 2, 3}}, F({{2017, 12, 24}, {1, 2, 3}}, 1))},
        {"add eight month in the middle of the year",
            ?_assertMatch({{2018, 2, 24}, {1, 2, 3}}, F({{2017, 6, 24}, {1, 2, 3}}, 8))},
        {"add twelve month in the middle of the year",
            ?_assertMatch({{2018, 5, 24}, {1, 2, 3}}, F({{2017, 5, 24}, {1, 2, 3}}, 12))}
    ].

%%----------------------------------------------------------------------
%% format/1
%%----------------------------------------------------------------------

format_test_() ->
    F = fun iso8601:format/1,
    [
        {"formats datetime with integer seconds",
            ?_assertEqual(<<"2012-02-03T04:05:06Z">>, F({{2012, 2, 3}, {4, 5, 6}}))},
        {"formats datetime with float seconds",
            ?_assertEqual(<<"2012-02-03T04:05:06.500000Z">>, F({{2012, 2, 3}, {4, 5, 6.5}}))},
        {"formats timestamp tuple", ?_assertEqual(<<"1970-01-01T00:00:00Z">>, F({0, 0, 0}))}
    ].

%%----------------------------------------------------------------------
%% Binary input dispatch
%%----------------------------------------------------------------------

parse_binary_test_() ->
    [
        {"parse/1 accepts binary input",
            ?_assertMatch({{2012, 1, 1}, {0, 0, 0}}, iso8601:parse(<<"2012">>))},
        {"parse_exact/1 accepts binary input",
            ?_assertMatch(
                {{2012, 2, 3}, {4, 5, 6.50}}, iso8601:parse_exact(<<"20120203T040506.50">>)
            )},
        {"parse_duration/1 accepts binary input",
            ?_assertMatch([{sign, []}, {years, 1} | _], iso8601:parse_duration(<<"P1Y">>))}
    ].

%%----------------------------------------------------------------------
%% Timestamp-tuple input clauses
%%----------------------------------------------------------------------

timestamp_input_test_() ->
    TS = {0, 0, 0},
    [
        {"add_time/4 accepts timestamp tuple",
            ?_assertMatch({{1970, 1, 1}, {1, 0, 0}}, iso8601:add_time(TS, 1, 0, 0))},
        {"add_days/2 accepts timestamp tuple",
            ?_assertMatch({{1970, 1, 2}, {0, 0, 0}}, iso8601:add_days(TS, 1))},
        {"add_months/2 accepts timestamp tuple",
            ?_assertMatch({{1970, 2, 1}, {0, 0, 0}}, iso8601:add_months(TS, 1))},
        {"add_years/2 accepts timestamp tuple",
            ?_assertMatch({{1971, 1, 1}, {0, 0, 0}}, iso8601:add_years(TS, 1))}
    ].

%%----------------------------------------------------------------------
%% add_days/2, add_years/2 (datetime input)
%%----------------------------------------------------------------------

add_days_test_() ->
    F = fun iso8601:add_days/2,
    [
        {"add one day",
            ?_assertMatch({{2017, 11, 29}, {1, 2, 3}}, F({{2017, 11, 28}, {1, 2, 3}}, 1))},
        {"add days across month boundary",
            ?_assertMatch({{2017, 12, 1}, {1, 2, 3}}, F({{2017, 11, 30}, {1, 2, 3}}, 1))}
    ].

add_years_test_() ->
    F = fun iso8601:add_years/2,
    [
        {"add one year",
            ?_assertMatch({{2018, 5, 24}, {1, 2, 3}}, F({{2017, 5, 24}, {1, 2, 3}}, 1))},
        {"add ten years",
            ?_assertMatch({{2027, 5, 24}, {1, 2, 3}}, F({{2017, 5, 24}, {1, 2, 3}}, 10))}
    ].

%%----------------------------------------------------------------------
%% add_months edge cases
%%----------------------------------------------------------------------

add_months_edge_test_() ->
    F = fun iso8601:add_months/2,
    [
        {"add zero months",
            ?_assertMatch({{2017, 5, 24}, {1, 2, 3}}, F({{2017, 5, 24}, {1, 2, 3}}, 0))},
        {"add month rolls back invalid day (Jan 31 + 1 month)",
            ?_assertMatch({{2017, 2, 28}, {1, 2, 3}}, F({{2017, 1, 31}, {1, 2, 3}}, 1))}
    ].

%%----------------------------------------------------------------------
%% apply_duration/2
%%----------------------------------------------------------------------

apply_duration_test_() ->
    F = fun iso8601:apply_duration/2,
    [
        {"apply duration with all components",
            ?_assertMatch(
                {{2018, 8, 1}, {2, 3, 4}}, F({{2017, 5, 24}, {1, 2, 3}}, "P1Y2M8DT1H1M1S")
            )},
        {"apply simple year duration",
            ?_assertMatch({{2018, 5, 24}, {1, 2, 3}}, F({{2017, 5, 24}, {1, 2, 3}}, "P1Y"))}
    ].

%%----------------------------------------------------------------------
%% date_at_w01_1 day-of-week branch coverage
%%----------------------------------------------------------------------

parse_week_dow_test_() ->
    F = fun iso8601:parse/1,
    [
        {"W01-1 year starting Monday (2018)", ?_assertMatch({{2018, 1, 1}, ?MN}, F("2018-W01-1"))},
        {"W01-1 year starting Tuesday (2019)",
            ?_assertMatch({{2018, 12, 31}, ?MN}, F("2019-W01-1"))},
        {"W01-1 year starting Wednesday (2014)",
            ?_assertMatch({{2013, 12, 30}, ?MN}, F("2014-W01-1"))},
        {"W01-1 year starting Friday (2010)", ?_assertMatch({{2010, 1, 4}, ?MN}, F("2010-W01-1"))},
        {"W01-1 year starting Saturday (2011)",
            ?_assertMatch({{2011, 1, 3}, ?MN}, F("2011-W01-1"))},
        {"W01-1 year starting Sunday (2012)", ?_assertMatch({{2012, 1, 2}, ?MN}, F("2012-W01-1"))}
    ].

%%----------------------------------------------------------------------
%% Offset formats: hour-only and colon-separated
%%----------------------------------------------------------------------

parse_offset_format_test_() ->
    F = fun iso8601:parse/1,
    [
        {"offset hour only (+04)",
            ?_assertMatch({{2012, 2, 3}, {8, 5, 6}}, F("2012-02-03T12:05:06+04"))},
        {"offset with colon (+04:30)",
            ?_assertMatch({{2012, 2, 3}, {8, 5, 6}}, F("2012-02-03T12:35:06+04:30"))}
    ].

%%----------------------------------------------------------------------
%% Colon-delimited fractional minutes and seconds
%%----------------------------------------------------------------------

parse_colon_fractional_minute_test_() ->
    F = fun iso8601:parse/1,
    [
        {"parses HH:MM.mm (dot)",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 15}}, F("2012-02-03T04:05.25"))},
        {"parses HH:MM,mm (comma)",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 15}}, F("2012-02-03T04:05,25"))}
    ].

parse_colon_fractional_second_test_() ->
    F = fun iso8601:parse/1,
    [
        {"parses HH:MM:SS.ss (dot)",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 6}}, F("2012-02-03T04:05:06.50"))},
        {"parses HH:MM:SS,ss (comma)",
            ?_assertMatch({{2012, 2, 3}, {4, 5, 6}}, F("2012-02-03T04:05:06,50"))}
    ].

%%----------------------------------------------------------------------
%% Parser error / fall-through clause coverage
%%----------------------------------------------------------------------

parse_error_clauses_test_() ->
    F = fun iso8601:parse/1,
    [
        {"year too short", ?_assertError(badarg, F("20"))},
        {"month_or_week trailing dash", ?_assertError(badarg, F("2012-"))},
        {"week_day bad char after week", ?_assertError(badarg, F("2009-W01X"))},
        {"month_day short day", ?_assertError(badarg, F("2012-12-1"))},
        {"month_day_no_hyphen single char day", ?_assertError(badarg, F("2012121"))},
        {"hour incomplete after T", ?_assertError(badarg, F("2012-12-12T"))},
        {"minute single char after hour", ?_assertError(badarg, F("2012-12-12T04:"))},
        {"second single char after colon minute", ?_assertError(badarg, F("2012-12-12T04:05:"))},
        {"second_no_colon single char", ?_assertError(badarg, F("20120203T0405X"))},
        {"decimal separator with no digits", ?_assertError(badarg, F("20120203T040506."))},
        {"offset_minute bad suffix", ?_assertError(badarg, F("2012-02-03T04:05:06+04X"))}
    ].
