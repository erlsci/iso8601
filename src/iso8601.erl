-module(iso8601).

-export([add_time/4,
         format/1,
         parse/1]).

-export_types([datetime/0]).

-define(MIDNIGHT, {0,0,0}).
-define(V, proplists:get_value).

-type datetime() :: {calendar:date(), calendar:time()}.

%% API

-spec add_time (datetime(), integer(), integer(), integer()) -> datetime().
%% @doc Add some time to the supplied `datetime()'.
add_time(Datetime, H, M, S) ->
    apply_offset(Datetime, H, M, S).

-spec format (util:timestamp()) -> binary().
%% @doc Convert a `util:timestamp()' or a calendar-style `{date(), time()}'
%% tuple to an ISO 8601 formatted string. Note that this function always
%% returns a string with no offset (i.e., ending in "Z").
format({_,_,_}=Timestamp) ->
    format(calendar:now_to_datetime(Timestamp));
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
    acc([Y1,Y2,Y3,Y4], Rest, year, Acc, fun month/2);
year(_, _) ->
    erlang:error(badarg).

month([], Acc) ->
    datetime(Acc);
month([$-,M1,M2|Rest], Acc) ->
    acc([M1,M2], Rest, month, Acc, fun day/2);
month(_, _) ->
    erlang:error(badarg).

day([], Acc) ->
    datetime(Acc);
day([$-,D1,D2|Rest], Acc) ->
    acc([D1,D2], Rest, day, Acc, fun hour/2);
day(_, _) ->
    erlang:error(badarg).

hour([], Acc) ->
    datetime(Acc);
hour([$T,H1,H2|Rest], Acc) ->
    acc([H1,H2], Rest, hour, Acc, fun minute/2);
hour(_, _) ->
    erlang:error(badarg).

minute([], Acc) ->
    datetime(Acc);
minute([$:,M1,M2|Rest], Acc) ->
    acc([M1,M2], Rest, minute, Acc, fun second/2);
minute(_, _) ->
    erlang:error(badarg).

second([], Acc) ->
    datetime(Acc);
second([$:,S1,S2|Rest], Acc) ->
    acc([S1,S2], Rest, second, Acc, fun offset_hour/2);
second(_, _) ->
    erlang:error(badarg).

offset_hour([], Acc) ->
    datetime(Acc);
offset_hour([$Z], Acc) ->
    acc([$0], [], offset, Acc, fun datetime/2);
offset_hour([$+,H1,H2|Rest], Acc) ->
    acc([H1,H2], Rest, offset_hour, Acc, fun offset_minute/2);
offset_hour([$-,H1,H2|Rest], Acc) ->
    acc([H1,H2], Rest, offset_hour, [{offset_sign, -1}|Acc], fun offset_minute/2);
offset_hour(_, _) ->
    erlang:error(badarg).

offset_minute([], Acc) ->
    datetime(Acc);
offset_minute([M1,M2], Acc) ->
    acc([M1,M2], [], offset_minute, Acc, fun datetime/2);
offset_minute([$:,M1,M2], Acc) ->
    acc([M1,M2], [], offset_minute, Acc, fun datetime/2);
offset_minute(_, _) ->
    erlang:error(badarg).

acc(IntStr, Rest, Key, Acc, NextF) ->
    Acc1 = [{Key, erlang:list_to_integer(IntStr)}|Acc],
    NextF(Rest, Acc1).

datetime(Plist) ->
    Year = ?V(year, Plist),
    Year =/= undefined orelse erlang:error(badarg),
    Date = {Year, ?V(month, Plist, 1), ?V(day, Plist, 1)},
    Time = {?V(hour, Plist, 0), ?V(minute, Plist, 0), ?V(second, Plist, 0)},
    OffsetSign = ?V(offset_sign, Plist, 1),
    OffsetH = OffsetSign * ?V(offset_hour, Plist, 0),
    OffsetM = OffsetSign * ?V(offset_minute, Plist, 0),
    apply_offset({Date, Time}, OffsetH, OffsetM, 0).

datetime(_, Plist) ->
    datetime(Plist).

apply_offset(Datetime, H, M, S) ->
    OffsetS = S + (60 * (M + (60 * H))),
    Gs = OffsetS + calendar:datetime_to_gregorian_seconds(Datetime),
    calendar:gregorian_seconds_to_datetime(Gs).
