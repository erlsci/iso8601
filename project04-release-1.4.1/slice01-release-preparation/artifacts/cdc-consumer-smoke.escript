#!/usr/bin/env escript
main([BeamDir]) ->
 true=code:add_patha(BeamDir),ok=application:load(iso8601),
 {ok,"1.4.1"}=application:get_key(iso8601,vsn),
 {ok,[kernel,stdlib]}=application:get_key(iso8601,applications),
 Path=code:which(iso8601),true=lists:prefix(filename:absname(BeamDir),filename:absname(Path)),
 io:format("application version1.4.1 dependencies kernel/stdlib module ~p~n",[Path]),
 Cases=[
  {invalid_week,fun()->iso8601:parse("2014-W53-1")end,{exception,error,badarg}},
  {valid_week,fun()->iso8601:parse("2015-W53-7")end,{return,{{2016,1,3},{0,0,0}}}},
  {nested_iodata,fun()->iso8601:parse([<<"2012-">>,["02",<<"-03">>]])end,{return,{{2012,2,3},{0,0,0}}}},
  {signed_interval,fun()->iso8601:parse_interval("+2007-02-15/03-14")end,{return,{interval,start_end,{{2007,2,15},{0,0,0}},{{2007,3,14},{0,0,0}}}}},
  {raw_zone,fun()->iso8601:interval_bounds(iso8601:parse_interval("2007-01-01T00:30:00+02:00/00:45:00"))end,{return,{{{2006,12,31},{22,30,0}},{{2006,12,31},{22,45,0}}}}},
  {zero_floor,fun()->iso8601:add_months({{1,1,1},{0,0,0}},-12)end,{return,{{0,1,1},{0,0,0}}}},
  {exact_composition,fun()->iso8601:format(iso8601:apply_duration(iso8601:parse_exact("2012-02-03T00.0001"),"PT0S"))end,{return,<<"2012-02-03T00:00:00.360000Z">>}}
 ],
 lists:foreach(fun({Name,Call,Expected})->Actual=outcome(Call),io:format("~p expected=~p actual=~p~n",[Name,Expected,Actual]),true=Actual=:=Expected end,Cases),
 {{2012,2,3},{0,0,F}}=iso8601:parse_exact("2012-02-03T00.0001"),true=abs(F-0.36)=<1.0e-12,io:format("exact fraction actual=~p expected0.36 tolerance1e-12 PASS~n",[F]),
 Parent=self(),Ref=make_ref(),{Pid,Mon}=spawn_monitor(fun()->Parent!{Ref,outcome(fun()->iso8601:add_months({{0,1,1},{0,0,0}},-1)end)}end),
 Result=receive {Ref,Value}->erlang:demonitor(Mon,[flush]),Value;{'DOWN',Mon,process,Pid,Why}->{unexpected_down,Why} after 500->exit(Pid,kill),receive {'DOWN',Mon,process,Pid,_}->{timeout,killed,down_confirmed} after 2000->{timeout,killed,down_missing}end end,
 io:format("bounded lowerfloor expected={exception,error,badarg} actual=~p~n",[Result]),{exception,error,badarg}=Result,
 io:format("PASS9literal controls plus application version/dependencies/codepath~n").
outcome(Call)->try {return,Call()}catch Class:Reason->{exception,Class,Reason}end.
