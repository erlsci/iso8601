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
      ?_assertMatch({{2012,2,3},{4,5,7}}, F("20120203T040506.50"))},
     {"parses YYYY-MM-DDTHHMMSS.ss",
      ?_assertMatch({{2012,2,3},{4,5,7}}, F("2012-02-03T040506.50"))},
     {"parses YYYYMMDDTHHMMSS,ss",
      ?_assertMatch({{2012,2,3},{4,5,7}}, F("20120203T040506,50"))},
     {"parses YYYY-MM-DDTHHMMSS,ss",
      ?_assertMatch({{2012,2,3},{4,5,7}}, F("2012-02-03T040506,50"))}].

parse_fractional_fail_test_() ->
    F = fun iso8601:parse/1,
    [{"fails to parses multiple decimals", % disallowed by spec
      ?_assertError(badarg, F("20120203T04.25:05.25:06"))}].

parse_offset_test_() ->
    F = fun iso8601:parse/1,
    [{"parses YYYYMMDDTHHMMSS.ssZ",
      ?_assertMatch({{2012,2,3},{4,5,7}}, F("20120203T040506.50Z"))},
     {"parses YYYYMMDDTHHMMSS.ss+0400",
      ?_assertMatch({{2012,2,3},{15,9,7}}, F("20120203T200506.50+0456"))},
     {"parses YYYYMMDDTHHMMSS.ss+0400",
      ?_assertMatch({{2012,2,3},{17,11,7}}, F("20120203T040506.50-1306"))}].

parse_duration_test_() ->
     F = fun iso8601:parse_duration/1,
     [{"parses with pos sign",
      ?_assertMatch([{sign,"+"},{years,6},{months,3},{days,1},
                     {hours,1},{minutes,1},{seconds,1}], 
                     F("+P6Y3M1DT1H1M1.1S"))},
     {"parses without sign",
      ?_assertMatch([{sign,[]},{years,6},{months,3},{days,1},
                     {hours,1},{minutes,1},{seconds,1}], 
                     F("P6Y3M1DT1H1M1.1S"))},
     {"parses only years",
      ?_assertMatch([{sign,[]},{years,6},{months,0},{days,0},
                     {hours,0},{minutes,0},{seconds,0}], 
                     F("P6Y"))},
     {"parses only minutes",
      ?_assertMatch([{sign,[]},{years,0},{months,0},{days,0},
                     {hours,0},{minutes,6},{seconds,0}], 
                     F("PT6M"))}].
      
parse_duration_fail_test_() ->
     F = fun iso8601:parse_duration/1,
     [{"fails to parses misspelled string", 
      ?_assertError(badarg, F("PIY"))}].
 
 parse_interval_test_() ->
     F = fun iso8601:parse_interval/1,
     [{"parses date duration with repeat",
      ?_assertMatch([{{2009,5,11},{15,30,0}},
                      {{2010,7,21},{18,0,0}},
                      {{2011,10,1},{20,30,0}},
                      {{2013,12,11},{23,0,0}},
                      {{2015,2,22},{1,30,0}}], 
                     F("R5/2008-03-01T13:00:00Z/P1Y2M10DT2H30M"))},
      {"parses  duration date with repeat",
      ?_assertMatch([{{2009,5,11},{15,30,0}},
                      {{2010,7,21},{18,0,0}},
                      {{2011,10,1},{20,30,0}},
                      {{2013,12,11},{23,0,0}},
                      {{2015,2,22},{1,30,0}}], 
                     F("R5/P1Y2M10DT2H30M/2008-03-01T13:00:00Z"))}
                     ].
%TODO test Repeat/duration
