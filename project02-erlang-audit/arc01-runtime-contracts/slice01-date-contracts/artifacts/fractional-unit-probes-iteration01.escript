#!/usr/bin/env escript
%% Iteration01 public-API checks for fractional hour/minute subsecond retention.
main(_) ->
    BeamDir = "/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit/_build/default/lib/iso8601/ebin",
    true = code:add_patha(BeamDir),
    Cases = [
        {hour_dot_0001, fun() -> iso8601:parse_exact("2012-02-03T04.0001") end,
            {{2012,2,3},{4,0,0.0001 * 3600}}},
        {hour_dot_0002, fun() -> iso8601:parse_exact("2012-02-03T04.0002") end,
            {{2012,2,3},{4,0,0.0002 * 3600}}},
        {minute_dot_001, fun() -> iso8601:parse_exact("2012-02-03T04:05.001") end,
            {{2012,2,3},{4,5,0.001 * 60}}},
        {hour_comma, fun() -> iso8601:parse_exact("2012-02-03T04,0001") end,
            {{2012,2,3},{4,0,0.0001 * 3600}}},
        {basic_binary_hour_comma,
            fun() -> iso8601:parse_exact(<<"20120203T04,0001">>) end,
            {{2012,2,3},{4,0,0.0001 * 3600}}},
        {minute_comma, fun() -> iso8601:parse_exact("2012-02-03T04:05,001") end,
            {{2012,2,3},{4,5,0.001 * 60}}},
        {expanded_hour_fraction,
            fun() -> iso8601:parse_expanded("+2012-02-03T04.0001") end,
            {{2012,2,3},{4,0,0.0001 * 3600}}},
        {hour_whole_second_control,
            fun() -> iso8601:parse_exact("2012-02-03T04.25") end,
            {{2012,2,3},{4,15,0.0}}},
        {explicit_second_control,
            fun() -> iso8601:parse_exact("2012-02-03T04:05:00.36") end,
            {{2012,2,3},{4,5,0.36}}},
        {explicit_second_comma_control,
            fun() -> iso8601:parse_exact("2012-02-03T04:05:00,36") end,
            {{2012,2,3},{4,5,0.36}}}
    ],
    Tolerance = 1.0e-12,
    lists:foreach(fun({Name, Call, Expected}) ->
        Actual = try {return, Call()} catch Class:Reason -> {exception, Class, Reason} end,
        Matches = case Actual of
            {return, {{Y,M,D},{H,Mi,S}}} ->
                {{Y,M,D},{H,Mi,ES}} = Expected,
                abs(S - ES) =< Tolerance;
            _ -> false
        end,
        io:format("~p expected=~p actual=~p matches_with_tolerance=~p tolerance=~p~n",
            [Name, Expected, Actual, Matches, Tolerance])
    end, Cases).
