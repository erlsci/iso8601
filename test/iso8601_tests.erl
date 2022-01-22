-module(iso8601_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MN, {0,0,0}). % midnight, as a calendar:time()
-define(MNE, {0,0,0.0}). % midnight, as a calendar:time(), with microseconds

parse_fail_test_() ->
    F = fun iso8601:parse/1,
    [{"fails to parse YYYYMM",
      ?_assertError(badarg, F("201212"))}].

parse_year_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYY",
      ?_assertMatch({{2012,1,1}, ?MN}, F("2012"))}].

parse_month_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYY-MM",
      ?_assertMatch({{2012,12,1}, ?MN}, F("2012-12"))}].

parse_month_day_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYYMMDD",
      ?_assertMatch({{2012,12,12}, ?MN}, F("2012-12-12"))},
     {"parses YYYY-MM-DD",
      ?_assertMatch({{2012,12,12}, ?MN}, F("20121212"))}].

parse_week_test_() ->
    F = fun iso8601:parse/1,
    [{"parses 2009W01 as 2008-12-29",
      ?_assertMatch({{2008,12,29}, ?MN}, F("2009W01"))},
     {"parses 2009-W01 as 2008-12-29",
      ?_assertMatch({{2008,12,29}, ?MN}, F("2009-W01"))},
     {"parses 2009W53 as 2010-01-03",
      ?_assertMatch({{2009,12,28}, ?MN}, F("2009W53"))},
     {"parses 2009-W53 as 2010-01-03",
      ?_assertMatch({{2009,12,28}, ?MN}, F("2009-W53"))}].

parse_week_day_test_() ->
    F = fun iso8601:parse/1,
    [{"parses 2009W011 as 2008-12-29",
      ?_assertMatch({{2008,12,29}, ?MN}, F("2009W011"))},
     {"parses 2009-W01-1 as 2008-12-29",
      ?_assertMatch({{2008,12,29}, ?MN}, F("2009-W01-1"))},
     {"parses 2009W537 as 2010-01-03",
      ?_assertMatch({{2010,1,3}, ?MN}, F("2009W537"))},
     {"parses 2009-W53-7 as 2010-01-03",
      ?_assertMatch({{2010,1,3}, ?MN}, F("2009-W53-7"))}].

parse_hour_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYYMMDDTHH",
      ?_assertMatch({{2012,2,3},{4,0,0}}, F("20120203T04"))},
     {"parses YYYY-MM-DDTHH",
      ?_assertMatch({{2012,2,3},{4,0,0}}, F("2012-02-03T04"))}].

parse_fractional_hour_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYYMMDDTHH.hh",
      ?_assertMatch({{2012,2,3},{4,15,0}}, F("20120203T04.25"))},
     {"parses YYYY-MM-DDTHH.hh",
      ?_assertMatch({{2012,2,3},{4,15,0}}, F("2012-02-03T04.25"))},
     {"parses YYYYMMDDTHH,hh",
      ?_assertMatch({{2012,2,3},{4,15,0}}, F("20120203T04,25"))},
     {"parses YYYY-MM-DDTHH,hh",
      ?_assertMatch({{2012,2,3},{4,15,0}}, F("2012-02-03T04,25"))}].

parse_minute_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYYMMDDTHHMM",
      ?_assertMatch({{2012,2,3},{4,5,0}}, F("20120203T0405"))},
     {"parses YYYY-MM-DDTHH:MM",
      ?_assertMatch({{2012,2,3},{4,5,0}}, F("2012-02-03T04:05"))}].

parse_fractional_minute_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYYMMDDTHHMM.mm",
      ?_assertMatch({{2012,2,3},{4,5,15}}, F("20120203T0405.25"))},
     {"parses YYYY-MM-DDTHHMM.mm",
      ?_assertMatch({{2012,2,3},{4,5,15}}, F("2012-02-03T0405.25"))},
     {"parses YYYYMMDDTHHMM,mm",
      ?_assertMatch({{2012,2,3},{4,5,15}}, F("20120203T0405,25"))},
     {"parses YYYY-MM-DDTHHMM,mm",
      ?_assertMatch({{2012,2,3},{4,5,15}}, F("2012-02-03T0405,25"))}].

parse_second_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYYMMDDTHHMMSS",
      ?_assertMatch({{2012,2,3},{4,5,6}}, F("20120203T040506"))},
     {"parses YYYY-MM-DDTHH:MM:SS",
      ?_assertMatch({{2012,2,3},{4,5,6}}, F("2012-02-03T04:05:06"))}].

parse_fractional_second_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYYMMDDTHHMMSS.ss",
      ?_assertMatch({{2012,2,3},{4,5,6}}, F("20120203T040506.50"))},
     {"parses YYYY-MM-DDTHHMMSS.ss",
      ?_assertMatch({{2012,2,3},{4,5,6}}, F("2012-02-03T040506.50"))},
     {"parses YYYYMMDDTHHMMSS,ss",
      ?_assertMatch({{2012,2,3},{4,5,6}}, F("20120203T040506,50"))},
     {"parses YYYY-MM-DDTHHMMSS,ss",
      ?_assertMatch({{2012,2,3},{4,5,6}}, F("2012-02-03T040506,50"))}].

parse_exact_fractional_second_test_() ->
    F = fun iso8601:parse_exact/1,
    [{"parses YYYYMMDDTHHMMSS.ss",
      ?_assertMatch({{2012,2,3},{4,5,6.50}}, F("20120203T040506.50"))},
     {"parses YYYY-MM-DDTHHMMSS.ss",
      ?_assertMatch({{2012,2,3},{4,5,6.50}}, F("2012-02-03T040506.50"))},
     {"parses YYYYMMDDTHHMMSS,ss",
      ?_assertMatch({{2012,2,3},{4,5,6.50}}, F("20120203T040506,50"))},
     {"parses YYYY-MM-DDTHHMMSS,ss",
      ?_assertMatch({{2012,2,3},{4,5,6.50}}, F("2012-02-03T040506,50"))}].

parse_fractional_fail_test_() ->
    F = fun iso8601:parse/1,
    [{"fails to parses multiple decimals", % disallowed by spec
      ?_assertError(badarg, F("20120203T04.25:05.25:06"))}].

parse_offset_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYYMMDDTHHMMSS.ssZ",
      ?_assertMatch({{2012,2,3},{4,5,6}}, F("20120203T040506.50Z"))},
     {"parses YYYYMMDDTHHMMSS.ss+0400",
      ?_assertMatch({{2012,2,3},{15,9,6}}, F("20120203T200506.50+0456"))},
     {"parses YYYYMMDDTHHMMSS.ss+0400",
      ?_assertMatch({{2012,2,3},{17,11,6}}, F("20120203T040506.50-1306"))}].

parse_duration_test_() ->
     F = fun iso8601:parse_duration/1,
     [{"parses with pos sign",
      ?_assertMatch([{sign, "+"}, {years, 6}, {months, 3}, {days, 1},
                     {hours, 1}, {minutes, 1}, {seconds, 1}],
                     F("+P6Y3M1DT1H1M1.1S"))},
     {"parses without sign",
      ?_assertMatch([{sign, []}, {years, 6}, {months, 3}, {days, 1},
                     {hours, 1}, {minutes, 1}, {seconds, 1}],
                     F("P6Y3M1DT1H1M1.1S"))},
     {"parses only years",
      ?_assertMatch([{sign, []}, {years, 6}, {months, 0}, {days, 0},
                     {hours, 0}, {minutes, 0}, {seconds, 0}],
                     F("P6Y"))},
     {"parses only minutes",
      ?_assertMatch([{sign, []}, {years, 0}, {months, 0}, {days, 0},
                     {hours, 0}, {minutes, 6}, {seconds, 0}],
                     F("PT6M"))}].

parse_duration_fail_test_() ->
     F = fun iso8601:parse_duration/1,
     [{"fails to parses misspelled string",
      ?_assertError(badarg, F("PIY"))}].

parse_ordinal_test_() ->
    F= fun iso8601:parse/1,
    [{"parses YYYY-DDDTHHMMSS", ?_assertMatch({{2016,2,3}, {4,5,6}}, F("2016-034T040506.50")) },
     {"parses YYYY-DDD", ?_assertMatch({{2016,2,3}, ?MN}, F("2016-034")) },
     {"parses YYYY_DDDTHHMMSS, Feb 29 in a leap year", ?_assertMatch({{2016,2,29}, {4,5,6}}, F("2016-060T040506.50")) },
     {"parses YYYY_DDDTHHMMSS, after Feb 29 in a leap year", ?_assertMatch({{2016,9,25}, {4,5,6}}, F("2016-269T040506.50")) },
     {"parses YYYY_DDDTHHMMSS, Feb 29 in a leap century", ?_assertMatch({{2000,2,29}, {4,5,6}}, F("2000-060T040506.50")) },
     {"parses YYYY_DDDTHHMMSS, March 1 in a non-leap year", ?_assertMatch({{2015,3,1}, {4,5,6}}, F("2015-060T040506.50")) },
     {"parses YYYY_DDDTHHMMSS, after Feb 29 in a non-leap year", ?_assertMatch({{2015,9,26}, {4,5,6}}, F("2015-269T040506.50")) },
     {"parses YYYY_DDDTHHMMSS, March 1 in a non-leap century", ?_assertMatch({{1900,3,1}, {4,5,6}}, F("1900-060T040506.50")) },
     {"fails to parse ordinal date with 0 days", ?_assertError(badarg, F("2016-000T040506.50"))},
     {"fails to parse ordinal date with too many days in a leap year", ?_assertError(badarg, F("2016-367T040506.50"))},
     {"parses ordinal date with 366 days in a leap year", ?_assertMatch({{2016,12,31}, {4,5,6}}, F("2016-366T040506.50"))},
     {"fails to parse ordinal date with too many days in a non-leap year", ?_assertError(badarg, F("2015-366T040506.50"))}
    ].

parse_ordinal_exact_test_() ->
    F= fun iso8601:parse_exact/1,
    [{"parses YYYY-DDDTHHMMSS", ?_assertMatch({{2016,2,3}, {4,5,6.50}}, F("2016-034T040506.50")) },
     {"parses YYYY-DDD", ?_assertMatch({{2016,2,3}, ?MNE}, F("2016-034")) },
     {"parses YYYY_DDDTHHMMSS, Feb 29 in a leap year", ?_assertMatch({{2016,2,29}, {4,5,6.50}}, F("2016-060T040506.50")) },
     {"parses YYYY_DDDTHHMMSS, after Feb 29 in a leap year", ?_assertMatch({{2016,9,25}, {4,5,6.50}}, F("2016-269T040506.50")) },
     {"parses YYYY_DDDTHHMMSS, Feb 29 in a leap century", ?_assertMatch({{2000,2,29}, {4,5,6.50}}, F("2000-060T040506.50")) },
     {"parses YYYY_DDDTHHMMSS, March 1 in a non-leap year", ?_assertMatch({{2015,3,1}, {4,5,6.50}}, F("2015-060T040506.50")) },
     {"parses YYYY_DDDTHHMMSS, after Feb 29 in a non-leap year", ?_assertMatch({{2015,9,26}, {4,5,6.50}}, F("2015-269T040506.50")) },
     {"parses YYYY_DDDTHHMMSS, March 1 in a non-leap century", ?_assertMatch({{1900,3,1}, {4,5,6.50}}, F("1900-060T040506.50")) },
     {"fails to parse ordinal date with 0 days", ?_assertError(badarg, F("2016-000T040506.50"))},
     {"fails to parse ordinal date with too many days in a leap year", ?_assertError(badarg, F("2016-367T040506.50"))},
     {"parses ordinal date with 366 days in a leap year", ?_assertMatch({{2016,12,31}, {4,5,6.50}}, F("2016-366T040506.50"))},
     {"fails to parse ordinal date with too many days in a non-leap year", ?_assertError(badarg, F("2015-366T040506.50"))}
    ].

add_time_test_() ->
    F= fun iso8601:add_time/4,
    [{"add one second", ?_assertMatch({{2017,11,28}, {17,7,58}}, F({{2017,11,28}, {17,7,57}}, 0, 0, 1))},
     {"add one minute", ?_assertMatch({{2017,11,28}, {17,8,57}}, F({{2017,11,28}, {17,7,57}}, 0, 1, 0))},
     {"add one hour", ?_assertMatch({{2017,11,28}, {18,7,57}}, F({{2017,11,28}, {17,7,57}}, 1, 0, 0))},
     {"roll over to next day", ?_assertMatch({{2017,11,29}, {00,30,00}}, F({{2017,11,28}, {23,30,00}}, 1, 0, 0))}
    ].

subtract_time_test_() ->
    F= fun iso8601:subtract_time/4,
    [{"add one second", ?_assertMatch({{2017,11,28}, {17,7,56}}, F({{2017,11,28}, {17,7,57}}, 0, 0, 1))},
     {"add one minute", ?_assertMatch({{2017,11,28}, {17,6,57}}, F({{2017,11,28}, {17,7,57}}, 0, 1, 0))},
     {"add one hour", ?_assertMatch({{2017,11,28}, {16,7,57}}, F({{2017,11,28}, {17,7,57}}, 1, 0, 0))},
     {"roll back to previous day", ?_assertMatch({{2017,11,28}, {23,30,00}}, F({{2017,11,29}, {00,30,00}}, 1, 0, 0))}
    ].

add_months_test_() ->
    F = fun iso8601:add_months/2,
    [{"add one month in the middle of the year", ?_assertMatch({{2017,6,24},{1,2,3}}, F({{2017,5,24},{1,2,3}}, 1))},
     {"add one month at the beginning of the year", ?_assertMatch({{2017,2,24},{1,2,3}}, F({{2017,1,24},{1,2,3}}, 1))},
     {"add one month almost in the end of the year", ?_assertMatch({{2017,12,24},{1,2,3}}, F({{2017,11,24},{1,2,3}}, 1))},
     {"add one month in the end of the year", ?_assertMatch({{2018,1,24},{1,2,3}}, F({{2017,12,24},{1,2,3}}, 1))},
     {"add eight month in the middle of the year", ?_assertMatch({{2018,2,24},{1,2,3}}, F({{2017,6,24},{1,2,3}}, 8))},
     {"add twelve month in the middle of the year", ?_assertMatch({{2018,5,24},{1,2,3}}, F({{2017,5,24},{1,2,3}}, 12))}
    ].
