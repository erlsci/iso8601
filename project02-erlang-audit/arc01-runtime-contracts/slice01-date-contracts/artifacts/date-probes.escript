#!/usr/bin/env escript
%% Reproducible Slice01 public-API probes; loads only the source checkout's fresh rebar beam.
main(_) ->
    BeamDir = "/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit/_build/default/lib/iso8601/ebin",
    true = code:add_patha(BeamDir),
    Cases = [
        {parse_baseline, fun() -> iso8601:parse("2012-02-16T01:06:48Z") end},
        {parse_exact_baseline, fun() -> iso8601:parse_exact("20120203T040506.50") end},
        {parse_positive_expanded, fun() -> iso8601:parse("+10000-01-01") end},
        {parse_expanded_negative_control, fun() -> iso8601:parse_expanded("-0001-01-01") end},
        {parse_rejects_negative_control, fun() -> iso8601:parse("-0001-01-01") end},
        {ordinal_leap_control, fun() -> iso8601:parse("2016-060") end},
        {week_boundary_control, fun() -> iso8601:parse("2009-W01-1") end},
        {week_last_valid_control, fun() -> iso8601:parse("2009-W53-7") end},
        {week_2015_w01_control, fun() -> iso8601:parse("2015-W01-1") end},
        {week_zero_invalid, fun() -> iso8601:parse("2014-W00-1") end},
        {week_53_invalid_for_2014, fun() -> iso8601:parse("2014-W53-1") end},
        {week_day_8_invalid, fun() -> iso8601:parse("2014-W01-8") end},
        {flat_list, fun() -> iso8601:parse("2012-02-16") end},
        {binary, fun() -> iso8601:parse(<<"2012-02-16">>) end},
        {nested_iodata_parse, fun() -> iso8601:parse(["2012-02-16", [$T, "01:06:48Z"]]) end},
        {nested_iodata_exact, fun() -> iso8601:parse_exact(["20120203", [$T, "040506.50"]]) end},
        {nested_iodata_expanded, fun() -> iso8601:parse_expanded(["-0001-01-01", [$T, "00:00:00Z"]]) end},
        {format_epoch_control, fun() -> iso8601:format({0, 0, 0}) end},
        {format_fraction_control, fun() -> iso8601:format({{2012,2,3},{4,5,6.5}}) end},
        {parse_regular_invalid_calendar_control, fun() -> iso8601:parse("2017-02-30") end},
        {parse_expanded_invalid_nonleap_feb29, fun() -> iso8601:parse_expanded("-0001-02-29") end},
        {parse_expanded_year_zero_valid_leap_control, fun() -> iso8601:parse_expanded("-0000-02-29") end},
        {parse_expanded_year_zero_invalid_feb30, fun() -> iso8601:parse_expanded("-0000-02-30") end},
        {parse_expanded_invalid_feb30, fun() -> iso8601:parse_expanded("-0001-02-30") end},
        {parse_expanded_invalid_month, fun() -> iso8601:parse_expanded("-0001-13-01") end},
        {parse_expanded_invalid_day_zero, fun() -> iso8601:parse_expanded("-0001-01-00") end},
        {parse_expanded_valid_negative_leap_control, fun() -> iso8601:parse_expanded("-0004-02-29") end},
        {format_float_below_roundover, fun() -> iso8601:format({{2012,12,30},{23,59,59.9999994}}) end},
        {format_float_roundover_nonleap_day, fun() -> iso8601:format({{2012,12,30},{23,59,59.9999999}}) end},
        {parse_format_roundover_nonleap_day, fun() -> iso8601:parse(iso8601:format({{2012,12,30},{23,59,59.9999999}})) end},
        {format_expanded_float_roundover_nonleap_day, fun() -> iso8601:format_expanded({{-1,12,30},{23,59,59.9999999}}) end}
    ],
    lists:foreach(fun({Name, Fun}) ->
        Value = try {return, Fun()} catch Class:Reason -> {exception, Class, Reason} end,
        io:format("~p => ~p~n", [Name, Value])
    end, Cases).
