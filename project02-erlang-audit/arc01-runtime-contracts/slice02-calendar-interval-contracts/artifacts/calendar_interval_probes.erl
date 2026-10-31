-module(calendar_interval_probes).
-export([run/0]).

%% Public-API probes for Slice02. Compile to an isolated beam directory.
run() ->
    probe("add_time day-boundary", fun() -> iso8601:add_time({{2017,12,31},{23,59,59}},0,0,1) end),
    probe("subtract_time control", fun() -> iso8601:subtract_time({{2018,1,1},{0,0,1}},0,0,1) end),
    probe("add_days leap transition", fun() -> iso8601:add_days({{2016,2,28},{12,0,0}},1) end),
    probe("add_months month-end clamp", fun() -> iso8601:add_months({{2017,1,31},{12,0,0}},1) end),
    probe("add_years leap-day clamp", fun() -> iso8601:add_years({{2016,2,29},{12,0,0}},1) end),
    probe("parse_duration signed ordered fields", fun() -> iso8601:parse_duration("-P1Y2M3DT4H5M6S") end),
    probe("apply_duration negative sign", fun() -> iso8601:apply_duration({{2017,5,24},{1,2,3}},"-P1Y") end),
    probe("apply_duration component-order witness", fun() -> iso8601:apply_duration({{2020,1,31},{0,0,0}},"P1M1D") end),
    probe("parse_interval start/end slash", fun() -> iso8601:parse_interval("2007-03-01T13:00:00Z/2008-05-11T15:30:00Z") end),
    probe("parse_interval start/duration", fun() -> iso8601:parse_interval("2007-03-01T13:00:00Z/P1Y2M") end),
    probe("parse_interval duration/end", fun() -> iso8601:parse_interval("-P1Y/2008-05-11T15:30:00Z") end),
    probe("parse_interval duration-only", fun() -> iso8601:parse_interval("P1Y") end),
    probe("interval_bounds start+duration", fun() -> iso8601:interval_bounds(iso8601:parse_interval("2007-03-01T13:00:00Z/P1Y2M10DT2H30M")) end),
    probe("interval_bounds negative duration/end", fun() -> iso8601:interval_bounds(iso8601:parse_interval("-P1Y/2008-05-11T15:30:00Z")) end),
    probe("interval_bounds duration-only policy", fun() -> iso8601:interval_bounds(iso8601:parse_interval("P1Y")) end),
    probe("format_interval canonical start/duration", fun() -> iso8601:format_interval(iso8601:parse_interval("2007-03-01T13:00:00Z/P1Y2M")) end),
    probe("format_interval duration/end", fun() -> iso8601:format_interval(iso8601:parse_interval("P1Y/2008-05-11T15:30:00Z")) end),
    probe("format_interval duration-only", fun() -> iso8601:format_interval(iso8601:parse_interval("P1Y")) end),
    probe("parse_interval legacy double-hyphen", fun() -> iso8601:parse_interval("2000--2002") end),
    probe("parse_interval inherited day", fun() -> iso8601:parse_interval("2007-11-13/15") end),
    probe("parse_interval inherited time UTC start", fun() -> iso8601:interval_bounds(iso8601:parse_interval("2007-12-14T13:30:00Z/15:30:00Z")) end),
    probe("parse_interval zoned full-second start and unzoned inherited time", fun() -> iso8601:interval_bounds(iso8601:parse_interval("2007-12-14T13:30:00+02:00/15:30:00")) end),
    probe("parse_interval zoned full-second start and explicitly zoned end", fun() -> iso8601:interval_bounds(iso8601:parse_interval("2007-12-14T13:30:00+02:00/15:30:00+02:00")) end),
    probe("parse_interval zoned minute-precision start", fun() -> iso8601:parse_interval("2007-12-14T13:30+02:00/15:30:00") end),
    probe("parse_interval malformed four-digit plus candidate", fun() -> iso8601:parse_interval("+2007-02-15/03-14") end),
    probe("parse_interval valid positive expanded inherited year", fun() -> iso8601:parse_interval("+0002007-02-15/03-14") end),
    probe("parse_interval valid positive expanded explicit-end control", fun() -> iso8601:parse_interval("+0002007-02-15/+0002007-03-14") end),
    probe("parse positive expanded standalone control", fun() -> iso8601:parse("+0002007-02-15") end),
    probe("parse_interval unsigned positive-year control", fun() -> iso8601:parse_interval("2007-02-15/03-14") end),
    probe("parse positive year control", fun() -> iso8601:parse("2007-02-15") end),
    probe("add_days fractional-second anchor", fun() -> iso8601:add_days({{2017,1,1},{0,0,0.5}},1) end),
    probe("apply_duration fractional-second anchor", fun() -> iso8601:apply_duration({{2017,1,1},{0,0,0.5}},"P1D") end),
    probe("interval_bounds fractional-second anchor", fun() -> iso8601:interval_bounds(iso8601:parse_interval("2017-01-01T00:00:00.5Z/P1D")) end),
    probe("parse_interval malformed two durations", fun() -> iso8601:parse_interval("P1Y/P2Y") end),
    probe("parse_interval malformed three pieces", fun() -> iso8601:parse_interval("2007/2008/2009") end),
    probe("ordinary negative-year add_years control", fun() -> iso8601:add_years({{4,2,29},{0,0,0}},-1) end),
    probe("calendar year-zero lower-bound control", fun() -> iso8601:add_years({{1,1,1},{0,0,0}},-1) end),
    probe("installed calendar validity lower bound", fun() -> {calendar:valid_date({0,1,1}),calendar:valid_date({-1,1,1}),calendar:valid_date({-1,1,0})} end),
    bounded("add_years underflow recursion hypothesis", fun() -> iso8601:add_years({{1,1,1},{0,0,0}},-2) end, 250),
    bounded("add_months underflow recursion hypothesis", fun() -> iso8601:add_months({{1,1,1},{0,0,0}},-24) end, 250),
    bounded("interval_bounds underflow recursion hypothesis", fun() -> iso8601:interval_bounds(iso8601:parse_interval("0001-01-01/-P2Y")) end, 250),
    bounded("apply_duration large negative-year recursion hypothesis", fun() -> iso8601:apply_duration({{2017,1,1},{0,0,0}},"-P9999Y") end, 250),
    ok.

probe(Label, Fun) ->
    io:format("~s => ~p~n", [Label, outcome(Fun)]).

outcome(Fun) ->
    try {return, Fun()} catch Class:Reason -> {exception, Class, Reason} end.

bounded(Label, Fun, Timeout) ->
    Parent = self(), Ref = make_ref(),
    {Pid, Mon} = spawn_monitor(fun() -> Parent ! {Ref, outcome(Fun)} end),
    Result = receive
        {Ref, Value} ->
            erlang:demonitor(Mon, [flush]),
            {returned, Value};
        {'DOWN', Mon, process, Pid, Reason} ->
            {down, Reason}
    after Timeout ->
        exit(Pid, kill),
        receive
            {'DOWN', Mon, process, Pid, _} -> {timeout, Timeout, killed, down_confirmed}
        after 2000 -> {timeout, Timeout, killed, down_not_observed}
        end
    end,
    io:format("~s => ~p~n", [Label, Result]).
