#!/usr/bin/env escript
main([BeamDir]) ->
    true = code:add_patha(BeamDir),
    DT = {{2024,2,28},{12,30,45}},
    Interval = {interval,start_end,{{2007,11,13},{0,0,0}},{{2007,11,15},{0,0,0}}},
    Dur = [{sign,[]},{years,0},{months,0},{days,1},{hours,0},{minutes,0},{seconds,0}],
    Cases = [
        {format, <<"2024-02-28T12:30:45Z">>, iso8601:format(DT)},
        {parse, DT, iso8601:parse(<<"2024-02-28T12:30:45Z">>)},
        {parse_exact, {{2024,2,28},{12,30,45.5}}, iso8601:parse_exact("2024-02-28T12:30:45.5Z")},
        {format_expanded, <<"-0044-03-15T00:00:00Z">>, iso8601:format_expanded({{-44,3,15},{0,0,0}})},
        {parse_expanded, {{-44,3,15},{0,0,0.0}}, iso8601:parse_expanded("-0044-03-15")},
        {add_time, {{2024,2,28},{13,30,45}}, iso8601:add_time(DT,1,0,0)},
        {subtract_time, {{2024,2,28},{11,30,45}}, iso8601:subtract_time(DT,1,0,0)},
        {add_days, {{2024,2,29},{12,30,45}}, iso8601:add_days(DT,1)},
        {add_months, {{2024,2,29},{0,0,0}}, iso8601:add_months({{2024,1,31},{0,0,0}},1)},
        {add_years, {{2025,2,28},{0,0,0}}, iso8601:add_years({{2024,2,29},{0,0,0}},1)},
        {parse_duration, Dur, iso8601:parse_duration("P1D")},
        {apply_duration, {{2023,12,31},{0,0,0}}, iso8601:apply_duration({{2024,1,1},{0,0,0}},"-P1D")},
        {parse_interval, Interval, iso8601:parse_interval("2007-11-13/15")},
        {interval_bounds, {{{2024,2,28},{0,0,0}},{{2024,2,29},{0,0,0}}}, iso8601:interval_bounds({interval,start_duration,{{2024,2,28},{0,0,0}},Dur})},
        {format_interval, <<"2007-11-13T00:00:00Z/2007-11-15T00:00:00Z">>, iso8601:format_interval(Interval)}
    ],
    lists:foreach(fun({Name,Expected,Actual}) ->
        case Expected =:= Actual of
            true -> io:format("PASS ~p~n",[Name]);
            false -> io:format("FAIL ~p expected=~p actual=~p~n",[Name,Expected,Actual]), halt(1)
        end
    end,Cases),
    io:format("PASS ~B/15 public API composition controls on OTP ~s~n",[length(Cases),erlang:system_info(otp_release)]).
