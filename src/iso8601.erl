-module(iso8601).

-export([add_time/4,
         format/1,
         parse/1]).

-export_types([datetime/0,
               timestamp/0]).

-define(MIDNIGHT, {0,0,0}).
-define(V, proplists:get_value).

-type datetime() :: {Date::calendar:date(),
                     Time::calendar:time()}.
-type datetime_plist() :: [{atom(), integer()}].
-type maybe(A) :: undefined | A.
-type timestamp() :: {MegaSecs::integer(),
                      Secs::integer(),
                      MicroSecs::integer() | float()}.

%% API

-spec add_time (datetime(), integer(), integer(), integer()) -> datetime().
%% @doc Add some time to the supplied `datetime()'.
add_time(Datetime, H, M, S) ->
    apply_offset(Datetime, H, M, S).

-spec format (datetime() | timestamp()) -> binary().
%% @doc Convert a `util:timestamp()' or a calendar-style `{date(), time()}'
%% tuple to an ISO 8601 formatted string. Note that this function always
%% returns a string with no offset (i.e., ending in "Z").
format({_,_,_}=Timestamp) ->
    format(calendar:now_to_datetime(Timestamp));
format({{Y,Mo,D}, {H,Mn,S}}) when is_float(S) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~9.6.0fZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    list_to_binary(IsoStr);
format({{Y,Mo,D}, {H,Mn,S}}) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    list_to_binary(IsoStr).

-spec parse (string()) -> datetime().
%% @doc Convert an ISO 8601 formatted string to a 
parse(Bin) when is_binary(Bin) ->
    parse(binary_to_list(Bin));
parse(Str) ->
    year(Str, []).

%% Private functions

year([Y1,Y2,Y3,Y4|Rest], Acc) ->
    acc([Y1,Y2,Y3,Y4], Rest, year, Acc, fun month_or_week/2);
year(_, _) ->
    error(badarg).

month_or_week([], Acc) ->
    datetime(Acc);
month_or_week([$-,$W,W1,W2|Rest], Acc) ->
    acc([W1,W2], Rest, week, Acc, fun week_day/2);
month_or_week([$-,M1,M2|Rest], Acc) ->
    acc([M1,M2], Rest, month, Acc, fun month_day/2);
month_or_week([$W,W1,W2|Rest], Acc) ->
    acc([W1,W2], Rest, week, Acc, fun week_day_no_hyphen/2);
month_or_week([M1,M2|Rest], Acc) ->
    acc([M1,M2], Rest, month, Acc, fun month_day_no_hyphen/2);
month_or_week(_, _) ->
    error(badarg).

week_day([], Acc) ->
    datetime(Acc);
week_day([$-,D|Rest], Acc) ->
    acc([D], Rest, week_day, Acc, fun hour/2);
week_day(_, _) ->
    error(badarg).

week_day_no_hyphen([], Acc) ->
    datetime(Acc);
week_day_no_hyphen([D|Rest], Acc) ->
    acc([D], Rest, week_day, Acc, fun hour/2);
week_day_no_hyphen(_, _) ->
    error(badarg).

month_day([], Acc) ->
    datetime(Acc);
month_day([$-,D1,D2|Rest], Acc) ->
    acc([D1,D2], Rest, month_day, Acc, fun hour/2);
month_day(_, _) ->
    error(badarg).

month_day_no_hyphen([], _) ->
    error(badarg); % omission of day disallowed by spec in this case
month_day_no_hyphen([D1,D2|Rest], Acc) ->
    acc([D1,D2], Rest, month_day, Acc, fun hour/2);
month_day_no_hyphen(_, _) ->
    error(badarg).

hour([], Acc) ->
    datetime(Acc);
hour([$T,H1,H2,$.|Rest], Acc) ->
    acc([H1,H2], Rest, hour, Acc, fun hour_decimal/2);
hour([$T,H1,H2,$,|Rest], Acc) ->
    acc([H1,H2], Rest, hour, Acc, fun hour_decimal/2);
hour([$T,H1,H2|Rest], Acc) ->
    acc([H1,H2], Rest, hour, Acc, fun minute/2);
hour(_, _) ->
    error(badarg).

hour_decimal(Str, Acc) ->
    decimal(Str, Acc, hour_decimal).

minute([], Acc) ->
    datetime(Acc);
minute([$:,M1,M2,$.|Rest], Acc) ->
    acc([M1,M2], Rest, minute, Acc, fun minute_decimal/2);
minute([$:,M1,M2,$,|Rest], Acc) ->
    acc([M1,M2], Rest, minute, Acc, fun minute_decimal/2);
minute([$:,M1,M2|Rest], Acc) ->
    acc([M1,M2], Rest, minute, Acc, fun second/2);
minute([M1,M2,$.|Rest], Acc) ->
    acc([M1,M2], Rest, minute, Acc, fun minute_decimal/2);
minute([M1,M2,$,|Rest], Acc) ->
    acc([M1,M2], Rest, minute, Acc, fun minute_decimal/2);
minute([M1,M2|Rest], Acc) ->
    acc([M1,M2], Rest, minute, Acc, fun second_no_colon/2);
minute(_, _) ->
    error(badarg).

minute_decimal(Str, Acc) ->
    decimal(Str, Acc, minute_decimal).

second([], Acc) ->
    datetime(Acc);
second([$:,S1,S2,$.|Rest], Acc) ->
    acc([S1,S2], Rest, second, Acc, fun second_decimal/2);
second([$:,S1,S2,$,|Rest], Acc) ->
    acc([S1,S2], Rest, second, Acc, fun second_decimal/2);
second([$:,S1,S2|Rest], Acc) ->
    acc([S1,S2], Rest, second, Acc, fun offset_hour/2);
second(_, _) ->
    error(badarg).

second_no_colon([], Acc) ->
    datetime(Acc);
second_no_colon([S1,S2,$.|Rest], Acc) ->
    acc([S1,S2], Rest, second, Acc, fun second_decimal/2);
second_no_colon([S1,S2,$,|Rest], Acc) ->
    acc([S1,S2], Rest, second, Acc, fun second_decimal/2);
second_no_colon([S1,S2|Rest], Acc) ->
    acc([S1,S2], Rest, second, Acc, fun offset_hour/2);
second_no_colon(_, _) ->
    error(badarg).

second_decimal(Str, Acc) ->
    decimal(Str, Acc, second_decimal).

decimal([], _, _) ->
    error(badarg);
decimal(Str, Acc, Key) ->
    F = fun(X) when is_integer(X), X >= $0, X =< $9 ->
                true;
           (_) ->
                false
        end,
    {Parts, Rest} = lists:splitwith(F, Str),
    acc_float([$0,$.|Parts], Rest, Key, Acc, fun offset_hour/2).    

offset_hour([], Acc) ->
    datetime(Acc);
offset_hour([$Z], Acc) ->
    datetime(Acc);
offset_hour([$+,H1,H2|Rest], Acc) ->
    acc([H1,H2], Rest, offset_hour, Acc, fun offset_minute/2);
offset_hour([$-,H1,H2|Rest], Acc) ->
    acc([H1,H2], Rest, offset_hour, [{offset_sign, -1}|Acc], fun offset_minute/2);
offset_hour(_, _) ->
    error(badarg).

offset_minute([], Acc) ->
    datetime(Acc);
offset_minute([$:,M1,M2], Acc) ->
    acc([M1,M2], [], offset_minute, Acc, fun datetime/2);
offset_minute([M1,M2], Acc) ->
    acc([M1,M2], [], offset_minute, Acc, fun datetime/2);
offset_minute(_, _) ->
    error(badarg).

acc(IntStr, Rest, Key, Acc, NextF) ->
    Acc1 = [{Key, list_to_integer(IntStr)}|Acc],
    NextF(Rest, Acc1).

acc_float(FloatStr, Rest, Key, Acc, NextF) ->
    Acc1 = [{Key, list_to_float(FloatStr)}|Acc],
    NextF(Rest, Acc1).

add_decimal(Datetime, Plist) ->
    HDecimal = ?V(hour_decimal, Plist, 0.0),
    MDecimal = ?V(minute_decimal, Plist, 0.0),
    SDecimal = ?V(second_decimal, Plist, 0.0),
    apply_offset(Datetime, HDecimal, MDecimal, SDecimal).

datetime(Plist) ->
    {Date, WeekOffsetH} = make_date(Plist),
    Time = {?V(hour, Plist, 0), ?V(minute, Plist, 0), ?V(second, Plist, 0)},
    Datetime = add_decimal({Date, Time}, Plist),
    OffsetSign = ?V(offset_sign, Plist, 1),
    OffsetH = -1 * OffsetSign * ?V(offset_hour, Plist, 0),
    OffsetM = -1 * OffsetSign * ?V(offset_minute, Plist, 0),
    apply_offset(Datetime, WeekOffsetH+OffsetH, OffsetM, 0).

datetime(_, Plist) ->
    datetime(Plist).

-spec make_date (datetime_plist())
                -> {Date::calendar:date(), WeekOffsetH::non_neg_integer()}.
%% @doc Return a `tuple' containing a date and, if the date is in week format,
%% an offset in hours that can be applied to the date to adjust it to midnight
%% of the day specified. If month format is used, the offset will be zero.
make_date(Plist) ->
    Year = ?V(year, Plist),
    Year =/= undefined orelse error(badarg),
    make_date(Year, ?V(month, Plist, 1), ?V(week, Plist), Plist).

-spec make_date (non_neg_integer(),
                 maybe(pos_integer()),
                 maybe(pos_integer()),
                 datetime_plist())
                -> {calendar:date(), non_neg_integer()}.
%% @doc Return a `tuple' containing a date and - if the date is in week format
%% (i.e., `Month' is undefined, `Week' is not) - an offset in hours that can be
%% applied to the date to adjust it to midnight of the day specified. If month
%% format is used (i.e., `Week' is undefined, `Month' is not), the offset will
%% be zero.
make_date(Year, Month, undefined, Plist) ->
    Date = {Year, Month, ?V(month_day, Plist, 1)},
    {Date, 0};
make_date(Year, _, Week, Plist) ->
    Weekday = ?V(week_day, Plist, 1),
    OffsetH = ((Week-1)*7 + (Weekday-1))*24, % week/weekday offset in hours
    {date_at_w01_1(Year), OffsetH}.

-spec date_at_w01_1(pos_integer()) -> calendar:date().
%% @doc Calculate the `calendar:date()' at ISO week 1, day 1 in the supplied
%% year.
date_at_w01_1(Year) ->
    case calendar:day_of_the_week({Year,1,1}) of
        1 -> {Year, 1, 1};
        2 -> {Year-1, 12, 31};
        3 -> {Year-1, 12, 30};
        4 -> {Year-1, 12, 29};
        5 -> {Year, 1, 4};
        6 -> {Year, 1, 3};
        7 -> {Year, 1, 2}
    end.

-spec apply_offset (datetime(), number(), number(), number()) -> datetime().
%% @doc Add the specified number of hours, minutes and seconds to `Datetime'.
%% Punts on sub-second precision for now by rounding the total of the number of
%% seconds in the offset before adding.
apply_offset(Datetime, H, M, S) ->
    OffsetS = S + (60 * (M + (60 * H))),
    Gs = round(OffsetS) + calendar:datetime_to_gregorian_seconds(Datetime),
    calendar:gregorian_seconds_to_datetime(Gs).
