#!/usr/bin/env escript
main([Ebin]) ->
    true = code:add_patha(Ebin),
    Cases = [
      {"short signed year/day", "+2007-11-13/15", {{2007,11,15},{0,0,0}}},
      {"padded signed year/month-day", "+0002007-02-15/03-14", {{2007,3,14},{0,0,0}}},
      {"large signed year/day-time", "+10000-11-13T09:00:00Z/15T17:00:00Z", {{10000,11,15},{17,0,0}}},
      {"full signed end", "2007-12-14/+10000-12-15", {{10000,12,15},{0,0,0}}},
      {"raw date across UTC boundary", "2007-01-01T00:30:00+02:00/00:45:00", {{2006,12,31},{22,45,0}}},
      {"negative offset inherited", "2007-12-14T13:30:00-02:00/15:30:00", {{2007,12,14},{17,30,0}}},
      {"explicit Z wins", "2007-12-14T13:30:00+02:00/15:30:00Z", {{2007,12,14},{15,30,0}}},
      {"explicit opposing offset wins", "2007-12-14T13:30:00+02:00/15:30:00-03:00", {{2007,12,14},{18,30,0}}},
      {"fraction retained", "2007-12-14T13:30:00+02:00/15:30:00.125", {{2007,12,14},{13,30,0.125}}},
      {"combined signed year and inherited zone", "+10000-01-01T00:30:00+02:00/T00:45:00", {{9999,12,31},{22,45,0}}},
      {"day-time inherits zone", "2007-11-13T09:00:00+02:00/15T17:00:00", {{2007,11,15},{15,0,0}}},
      {"minute fragment inherits zone", "2007-12-14T13:30:00+02:00/15:30", {{2007,12,14},{13,30,0}}},
      {"unsigned control", "2007-11-13/15", {{2007,11,15},{0,0,0}}},
      {"zone-free control", "2007-12-14T13:30/15:30", {{2007,12,14},{15,30,0}}},
      {"full unsigned endpoint control", "2007-11-13/2008-01-01", {{2008,1,1},{0,0,0}}}
    ],
    Fails = lists:foldl(fun({Name,Str,Want},N) ->
        Got = try element(2,iso8601:interval_bounds(iso8601:parse_interval(Str))) catch C:R -> {raised,C,R} end,
        case Got =:= Want of
          true -> io:format("PASS ~s~n",[Name]), N;
          false -> io:format("FAIL ~s expected=~p actual=~p~n",[Name,Want,Got]), N+1
        end
    end,0,Cases),
    Negatives = ["20071113/15","2008-02-15/03-14T10:00","2007-11-13/9","2007-11-13/-0001-01-01"],
    NFails = lists:foldl(fun(Str,N) ->
        Got = try iso8601:parse_interval(Str), accepted catch error:badarg -> rejected; C:R -> {raised,C,R} end,
        case Got of rejected -> io:format("PASS rejection ~s~n",[Str]),N; _ -> io:format("FAIL rejection ~s actual=~p~n",[Str,Got]),N+1 end
    end,0,Negatives),
    io:format("SUMMARY ~p checks ~p failures~n",[length(Cases)+length(Negatives),Fails+NFails]),
    case Fails+NFails of 0 -> halt(0); _ -> halt(1) end.
