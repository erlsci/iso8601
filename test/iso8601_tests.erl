-module(iso8601_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(MN, {0,0,0}). % midnight, as a calendar:time()

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

parse_minute_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYYMMDDTHHMM",
      ?_assertMatch({{2012,2,3},{4,5,0}}, F("20120203T0405"))},
     {"parses YYYY-MM-DDTHH:MM",
      ?_assertMatch({{2012,2,3},{4,5,0}}, F("2012-02-03T04:05"))}].    

parse_second_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYYMMDDTHHMMSS",
      ?_assertMatch({{2012,2,3},{4,5,6}}, F("20120203T040506"))},
     {"parses YYYY-MM-DDTHH:MM:SS",
      ?_assertMatch({{2012,2,3},{4,5,6}}, F("2012-02-03T04:05:06"))}].    
