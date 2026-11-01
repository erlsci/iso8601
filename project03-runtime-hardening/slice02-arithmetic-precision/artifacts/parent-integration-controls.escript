#!/usr/bin/env escript
main([BeamDir]) ->
    true = code:add_patha(BeamDir),
    Cases = [
      {month_wrap, fun() -> iso8601:add_months({{0,1,1},{0,0,0}},-1) end, {exception,error,badarg}},
      {year_underflow, fun() -> iso8601:add_years({{1,1,1},{0,0,0}},-2) end, {exception,error,badarg}},
      {duration_underflow, fun() -> iso8601:apply_duration({{2017,1,1},{0,0,0}},"-P9999Y") end, {exception,error,badarg}},
      {valid_zero_target, fun() -> iso8601:add_months({{1,1,1},{0,0,0}},-12) end, {return,{{0,1,1},{0,0,0}}}},
      {float_zero_duration, fun() -> iso8601:apply_duration(iso8601:parse_exact("2017-01-01"),"P1D") end, {return,{{2017,1,2},{0,0,0.0}}}},
      {float_micro_large_year, fun() -> iso8601:apply_duration({{100000,1,1},{0,0,0.000001}},"P1D") end, {return,{{100000,1,2},{0,0,0.000001}}}},
      {duration_end_float, fun() -> iso8601:interval_bounds(iso8601:parse_interval("P1D/2017-01-02T00:00:00.5Z")) end, {return,{{{2017,1,1},{0,0,0.5}},{{2017,1,2},{0,0,0.5}}}}},
      {hour_exact, fun() -> iso8601:parse_exact("2012-02-03T04.0001") end, {approx,{{2012,2,3},{4,0,0.36}}}},
      {minute_exact, fun() -> iso8601:parse_exact("2012-02-03T04:05.001") end, {approx,{{2012,2,3},{4,5,0.06}}}},
      {hour_legacy_round, fun() -> iso8601:parse("2012-02-03T04.0002") end, {return,{{2012,2,3},{4,0,1}}}},
      {second_legacy_trunc, fun() -> iso8601:parse("20120203T040506.50") end, {return,{{2012,2,3},{4,5,6}}}},
      {float_formatter_carry, fun() -> iso8601:format({{2012,12,30},{23,59,59.9999999}}) end, {return,<<"2012-12-31T00:00:00.000000Z">>}},
      {expanded_negative_year_carry, fun() -> iso8601:format_expanded({{-1,12,31},{23,59,59.9999999}}) end, {return,<<"+0000-01-01T00:00:00.000000Z">>}},
      {year_zero_time_policy, fun() -> iso8601:add_time({{0,1,1},{0,0,0}},1,0,0) end, {exception,error,badarg}},
      {negative_unit_offset_policy, fun() -> iso8601:parse_expanded("-0001-01-01T04.0002") end, {exception,error,badarg}},
      {negative_plain_control, fun() -> iso8601:parse_expanded("-0001-01-01") end, {return,{{-1,1,1},{0,0,0.0}}}}
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
