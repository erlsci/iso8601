#!/usr/bin/env escript
main([BeamDir]) ->
    true = code:add_patha(BeamDir),
    Cases = [
      {invalid_week_zero, fun() -> iso8601:parse("2014-W00-1") end, {exception,error,badarg}},
      {invalid_week_53, fun() -> iso8601:parse("2014-W53-1") end, {exception,error,badarg}},
      {invalid_weekday_zero, fun() -> iso8601:parse("2017-W01-0") end, {exception,error,badarg}},
      {invalid_weekday_eight, fun() -> iso8601:parse("2017-W01-8") end, {exception,error,badarg}},
      {valid_week_53, fun() -> iso8601:parse("2015-W53-7") end, {return,{{2016,1,3},{0,0,0}}}},
      {valid_week_cross_year, fun() -> iso8601:parse("2015-W01-1") end, {return,{{2014,12,29},{0,0,0}}}},
      {year_zero_week_control, fun() -> iso8601:parse_exact("0000-W01-1") end, {return,{{0,1,3},{0,0,0.0}}}},
      {negative_week_restriction, fun() -> iso8601:parse_expanded("-0004-W01-1") end, {exception,error,badarg}},
      {negative_nonleap_day, fun() -> iso8601:parse_expanded("-0001-02-29") end, {exception,error,badarg}},
      {year_zero_impossible_day, fun() -> iso8601:parse("0000-02-30") end, {exception,error,badarg}},
      {negative_century_nonleap, fun() -> iso8601:parse_expanded("-0100-02-29") end, {exception,error,badarg}},
      {negative_400_leap_control, fun() -> iso8601:parse_expanded("-0400-02-29") end, {return,{{-400,2,29},{0,0,0.0}}}},
      {year_zero_leap_control, fun() -> iso8601:parse_exact("0000-02-29") end, {return,{{0,2,29},{0,0,0.0}}}},
      {negative_zone_restriction, fun() -> iso8601:parse_expanded("-0004-02-29T00:00:00+01:00") end, {exception,error,badarg}},
      {nested_legacy, fun() -> iso8601:parse(["2012-02-16",[$T,"01:06:48Z"]]) end, {return,{{2012,2,16},{1,6,48}}}},
      {nested_exact_unit, fun() -> iso8601:parse_exact(["2012-02-03",<<"T04.0001Z">>]) end, {approx,{{2012,2,3},{4,0,0.36}}}},
      {nested_expanded_leap, fun() -> iso8601:parse_expanded([<<"-0004">>,["-02-29"]]) end, {return,{{-4,2,29},{0,0,0.0}}}},
      {binary_tail_exact, fun() -> iso8601:parse_exact(["2017" | <<"-01-01">>]) end, {return,{{2017,1,1},{0,0,0.0}}}},
      {nested_interval_inheritance, fun() -> iso8601:interval_bounds(iso8601:parse_interval([<<"+10000-01-01T00:30:00+02:00">>,["/",<<"02T00:15:00">>]])) end, {return,{{{9999,12,31},{22,30,0}},{{10000,1,1},{22,15,0}}}}},
      {nested_duration_interval, fun() -> iso8601:format_interval(iso8601:parse_interval([<<"P1D">>])) end, {return,<<"P1D">>}},
      {nonbyte_iodata, fun() -> iso8601:parse([<<"2017-01-01">>,[256]]) end, {exception,error,badarg}},
      {atom_iodata, fun() -> iso8601:parse_exact([<<"2017-01-01">>,invalid]) end, {exception,error,badarg}},
      {atom_tail_iodata, fun() -> iso8601:parse_expanded(["2017" | invalid]) end, {exception,error,badarg}},
      {bitstring_iodata, fun() -> iso8601:parse_interval(<<0:1>>) end, {exception,error,badarg}}
    ],
    Results = [begin O = observe(F), OK = matches(O,E), io:format("~p => ~p expected ~p pass ~p~n",[Name,O,E,OK]), OK end || {Name,F,E} <- Cases],
    io:format("total ~p passed ~p failed ~p~n",[length(Results),length([true || true <- Results]),length([false || false <- Results])]).
observe(F) ->
    Parent = self(), Ref = make_ref(),
    {Pid,Monitor} = spawn_monitor(fun() -> O = try F() of V -> {return,V} catch C:R -> {exception,C,R} end, Parent ! {Ref,O} end),
    receive {Ref,O} -> receive {'DOWN',Monitor,process,Pid,_} -> O end
    after 500 -> exit(Pid,kill), receive {'DOWN',Monitor,process,Pid,killed} -> timeout end end.
matches({return,{D,{H,M,S}}},{approx,{D,{H,M,E}}}) -> abs(S-E) < 1.0e-12;
matches(O,E) -> O =:= E.
