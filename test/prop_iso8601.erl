-module(prop_iso8601).

-include_lib("proper/include/proper.hrl").

prop_parse_format_roundtrip() ->
    ?FORALL(
        {Y, Mo, D, H, Mn, S},
        {range(1, 9999), range(1, 12), range(1, 28), range(0, 23), range(0, 59), range(0, 59)},
        begin
            DT = {{Y, Mo, D}, {H, Mn, S}},
            DT =:= iso8601:parse(iso8601:format(DT))
        end
    ).

prop_interval_start_end_roundtrip() ->
    ?FORALL(
        {Y1, Mo1, D1, H1, Mn1, S1, Y2, Mo2, D2, H2, Mn2, S2},
        {range(1, 9999), range(1, 12), range(1, 28), range(0, 23), range(0, 59), range(0, 59),
         range(1, 9999), range(1, 12), range(1, 28), range(0, 23), range(0, 59), range(0, 59)},
        begin
            Start = {{Y1, Mo1, D1}, {H1, Mn1, S1}},
            End = {{Y2, Mo2, D2}, {H2, Mn2, S2}},
            I = {interval, start_end, Start, End},
            I =:= iso8601:parse_interval(binary_to_list(iso8601:format_interval(I)))
        end
    ).

prop_start_duration_bounds() ->
    ?FORALL(
        {Y, Mo, D, H, Mn, S, DY, DM, DD, DH, DMi, DS},
        {range(1, 5000), range(1, 12), range(1, 28), range(0, 23), range(0, 59), range(0, 59),
         range(0, 10), range(0, 11), range(0, 28), range(0, 23), range(0, 59), range(0, 59)},
        begin
            Start = {{Y, Mo, D}, {H, Mn, S}},
            Dur = [{sign, ""}, {years, DY}, {months, DM}, {days, DD},
                   {hours, DH}, {minutes, DMi}, {seconds, DS}],
            I = {interval, start_duration, Start, Dur},
            {_, End} = iso8601:interval_bounds(I),
            End =:= iso8601:apply_duration(Start,
                binary_to_list(iolist_to_binary(format_dur(Dur))))
        end
    ).

format_dur(Plist) ->
    Y = proplists:get_value(years, Plist, 0),
    Mo = proplists:get_value(months, Plist, 0),
    D = proplists:get_value(days, Plist, 0),
    H = proplists:get_value(hours, Plist, 0),
    Mi = proplists:get_value(minutes, Plist, 0),
    S = proplists:get_value(seconds, Plist, 0),
    DateParts = [integer_to_list(V) ++ U ||
        {V, U} <- [{Y, "Y"}, {Mo, "M"}, {D, "D"}], V > 0],
    TimeParts = [integer_to_list(V) ++ U ||
        {V, U} <- [{H, "H"}, {Mi, "M"}, {S, "S"}], V > 0],
    TimeSec = case TimeParts of
        [] -> "";
        _ -> "T" ++ lists:flatten(TimeParts)
    end,
    lists:flatten(["P", lists:flatten(DateParts), TimeSec]).

prop_expanded_roundtrip() ->
    ?FORALL(
        {Y, Mo, D, H, Mn, S},
        {range(-9999, 9999), range(1, 12), range(1, 28),
         range(0, 23), range(0, 59), range(0, 59)},
        begin
            DT = {{Y, Mo, D}, {H, Mn, S}},
            Formatted = binary_to_list(iso8601:format_expanded(DT)),
            {{PY, PMo, PD}, {PH, PMn, PS}} = iso8601:parse_expanded(Formatted),
            Y =:= PY andalso Mo =:= PMo andalso D =:= PD andalso
            H =:= PH andalso Mn =:= PMn andalso S =:= round(PS)
        end
    ).
