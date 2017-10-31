-module(prop_tests).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%% Format a datetime, then parse it back out and get the same value
prop_format_roundtrip() ->
    ?FORALL(Datetime, datetime_tuple(), check_format_roundtrip(Datetime)).

%% Format a datetime, then check that it has the expected number of chars
prop_format_length() ->
    ?FORALL(Datetime, datetime_tuple(), check_format_length(Datetime)).

%% Format a timestamp, then check that it has the expected number of chars
prop_format_timestamp() ->
    ?FORALL(ErlangTimestamp, erlang_timestamp(), check_format_length(ErlangTimestamp)).

%% Parse a YYYY-MM-DD date and check that it's correct
prop_parse_date() ->
    ?FORALL(DateTupleString, date_tuple_string(), check_parse_date_tuple(DateTupleString)).

%% Parse a datetime string without crashing
prop_parse_datetime() ->
    ?FORALL(DatetimeString, datetime_string(), check_parse_datetime(DatetimeString)).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%% the library does not work on years < 0 or > 9999
-define(ERAS, [modern, pre_unix, pre_gregorian]).

check_format_roundtrip(Datetime) ->
    Binary = iso8601:format(Datetime),
    iso8601:parse(Binary) =:= Datetime.

check_format_length(Datetime) ->
    Binary = iso8601:format(Datetime),
    byte_size(Binary) =:= 20.

check_parse_date_tuple({Year, Month, Day}) ->
    Binary = iolist_to_binary([Year, $-, Month, $-, Day]),
    YearI = list_to_integer(lists:flatten(Year)),
    MonthI = list_to_integer(lists:flatten(Month)),
    DayI = list_to_integer(lists:flatten(Day)),
    {{YearI, MonthI, DayI}, {0, 0, 0}} =:= iso8601:parse(Binary).

check_parse_datetime(DatetimeString) ->
    Binary = iolist_to_binary(DatetimeString),
    case iso8601:parse(Binary) of
        {{_, _, _}, {_, _, _}} ->
            true;
        _ ->
            false
    end.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

%% Separate years into categories:
%%  Basic ISO8601 is [modern, pre_unix]
%%  Pre-Gregorian doesn't represent how anyone thought of dates in that time
%%  Ancient is BCE and needs a '-' prefix
%%  Future needs 5 digits and a '+' prefix
year_number(modern) ->
    integer(1970, 9999);
year_number(pre_unix) ->
    integer(1583, 1969);
year_number(pre_gregorian) ->
    integer(0, 1582);
year_number(future) ->
    integer(10000, 99999);
year_number(ancient) ->
    neg_integer().

year_string(future) ->
    ?LET(Year, year_number(future), lists:flatten(io_lib:format("+~5..0B", [Year])));
year_string(ancient) ->
    ?LET(Year, year_number(ancient), lists:flatten(io_lib:format("-~5..0B", [abs(Year)])));
year_string(FourDigit) ->
    ?LET(Year, year_number(FourDigit), lists:flatten(io_lib:format("~4..0B", [Year]))).

month_number() ->
    integer(1, 12).

month_string() ->
    ?LET(Month, month_number(), lists:flatten(io_lib:format("~2..0B", [Month]))).

%% three functions for the 4 different possible month-lengths
day_number(Year, 2) when is_integer(Year) ->
    case calendar:is_leap_year(Year) of
        true ->
            integer(1, 29);
        false ->
            integer(1, 28)
    end;
day_number(_Year, Month) when Month =:= 4 orelse Month =:= 6 orelse Month =:= 9 orelse Month =:= 11 ->
    integer(1, 30);
day_number(_Year, _Month) when is_integer(_Year), is_integer(_Month) ->
    integer(1, 31).

day_string(Year, Month) ->
    ?LET(Day, day_number(list_to_integer(Year), list_to_integer(Month)), lists:flatten(io_lib:format("~2..0B", [Day]))).

date_tuple(Era) ->
    ?LET({Year, Month}, {year_number(Era), month_number()}, {Year, Month, day_number(Year, Month)}).

date_tuple_string(Era) ->
    ?LET({Year, Month}, {year_string(Era), month_string()}, {Year, Month, day_string(Year, Month)}).
date_tuple_string() ->
    union(lists:map(fun date_tuple_string/1, ?ERAS)).

date_string(Era) ->
    ?LET({Year, Month}, {year_string(Era), month_string()}, lists:flatten([Year, $-, Month, $-, day_string(Year, Month)])).
date_string() ->
    union(lists:map(fun date_string/1, ?ERAS)).

hour_number() ->
    integer(0, 23).

hour_string() ->
    ?LET(Hour, hour_number(), lists:flatten(io_lib:format("~2..0B", [Hour]))).

minute_number() ->
    integer(0, 59).

minute_string() ->
    ?LET(Minute, minute_number(), lists:flatten(io_lib:format("~2..0B", [Minute]))).

second_number() ->
    integer(0, 59).

second_string() ->
    ?LET(Second, second_number(), lists:flatten(io_lib:format("~2..0B", [Second]))).

time_tuple() ->
    {hour_number(), minute_number(), second_number()}.

pos_neg_string() ->
    union(["+", "-"]).

time_zone_4_digit_sep_string() ->
    ?LET({Sign, Hour, Minute}, {pos_neg_string(), hour_string(), minute_string()}, lists:flatten([Sign, Hour, $:, Minute])).

time_zone_4_digit_nosep_string() ->
    ?LET({Sign, Hour, Minute}, {pos_neg_string(), hour_string(), minute_string()}, lists:flatten([Sign, Hour, Minute])).

time_zone_2_digit_string() ->
    ?LET({Sign, Hour}, {pos_neg_string(), hour_string()}, lists:flatten([Sign, Hour])).

time_zone_string() ->
    union([
           "Z"
          ,time_zone_4_digit_sep_string()
          ,time_zone_4_digit_nosep_string()
          ,time_zone_2_digit_string()
          ]).

time_string() ->
    ?LET({{Hour, Minute, Second}, Zone}, {time_tuple(), time_zone_string()}, lists:flatten(io_lib:format("~2..0B:~2..0B:~2..0B~s", [Hour, Minute, Second, Zone]))).

datetime_tuple(Era) ->
    {date_tuple(Era), time_tuple()}.
datetime_tuple() ->
    union(lists:map(fun datetime_tuple/1, ?ERAS)).

datetime_separator_string() ->
    union(["T"]).  % Other separators: " ", ""

datetime_string(Era) ->
    ?LET({Date, Separator, Time}, {date_string(Era), datetime_separator_string(), time_string()}, lists:flatten([Date, Separator, Time])).
datetime_string() ->
    union(lists:map(fun datetime_string/1, ?ERAS)).

erlang_timestamp() ->
    {integer(), integer(), integer()}.
