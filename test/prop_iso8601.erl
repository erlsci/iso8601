-module(prop_iso8601).

-include_lib("proper/include/proper.hrl").

prop_parse_format_roundtrip() ->
    ?FORALL({Y, Mo, D, H, Mn, S},
            {range(1, 9999), range(1, 12), range(1, 28),
             range(0, 23), range(0, 59), range(0, 59)},
            begin
                DT = {{Y, Mo, D}, {H, Mn, S}},
                DT =:= iso8601:parse(iso8601:format(DT))
            end).
