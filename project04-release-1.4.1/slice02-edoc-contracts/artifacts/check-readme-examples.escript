#!/usr/bin/env escript
%%! -noshell

main([BeamDir]) ->
    true = code:add_patha(BeamDir),
    Checks = [
        {format_epoch, <<"1970-01-01T00:00:00Z">>, fun() -> iso8601:format({0, 0, 0}) end},
        {format_datetime, <<"2012-02-16T01:06:48Z">>, fun() ->
            iso8601:format({{2012, 2, 16}, {1, 6, 48}})
        end},
        {nested_parse, {{2012, 2, 3}, {0, 0, 0}}, fun() ->
            iso8601:parse([<<"2012-">>, ["02", <<"-03">>]])
        end},
        {add_time, {{2012, 2, 16}, {2, 8, 51}}, fun() ->
            iso8601:add_time({{2012, 2, 16}, {1, 6, 48}}, 1, 2, 3)
        end},
        {subtract_time, {{2012, 2, 16}, {0, 4, 45}}, fun() ->
            iso8601:subtract_time({{2012, 2, 16}, {1, 6, 48}}, 1, 2, 3)
        end},
        {legacy_fraction, {{2012, 2, 3}, {4, 5, 6}}, fun() ->
            iso8601:parse("20120203T040506.50")
        end},
        {duration_nested, [{sign, []}, {years, 0}, {months, 0}, {days, 0},
            {hours, 0}, {minutes, 6}, {seconds, 0}], fun() ->
            iso8601:parse_duration([<<"PT">>, ["6", <<"M">>]])
        end},
        {add_days, {{2017, 2, 1}, {12, 0, 0.25}}, fun() ->
            iso8601:add_days({{2017, 1, 31}, {12, 0, 0.25}}, 1)
        end},
        {add_months, {{2017, 2, 28}, {12, 0, 0.25}}, fun() ->
            iso8601:add_months({{2017, 1, 31}, {12, 0, 0.25}}, 1)
        end},
        {add_years, {{2017, 2, 28}, {12, 0, 0.25}}, fun() ->
            iso8601:add_years({{2016, 2, 29}, {12, 0, 0.25}}, 1)
        end},
        {apply_duration, {{2017, 2, 28}, {12, 0, 0.25}}, fun() ->
            iso8601:apply_duration({{2017, 1, 31}, {12, 0, 0.25}}, <<"P1M">>)
        end}
    ],
    lists:foreach(
        fun({Name, Expected, Call}) ->
            Actual = Call(),
            io:format("~p expected=~p actual=~p~n", [Name, Expected, Actual]),
            Expected = Actual
        end,
        Checks
    ),
    io:format("PASS ~B deterministic README examples~n", [length(Checks)]).
