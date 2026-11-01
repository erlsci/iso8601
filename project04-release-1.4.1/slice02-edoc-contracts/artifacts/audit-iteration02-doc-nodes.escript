#!/usr/bin/env escript
%%! -noshell

main([Chunk]) ->
    {ok, Bytes} = file:read_file(Chunk),
    {docs_v1, _, _, _, _, _, Entries} = binary_to_term(Bytes),
    Year = doc(Entries, {type, expanded_year, 0}),
    AddYears = doc(Entries, {function, add_years, 2}),
    Parse = doc(Entries, {function, parse, 1}),
    Interval = doc(Entries, {function, parse_interval, 1}),
    YearText = text(Year),
    AddYearsText = text(AddYears),
    ParseText = text(Parse),
    IntervalText = text(Interval),
    true = contains(YearText, <<"year 0 is 1 BCE and year -1 is 2 BCE">>),
    true = contains(AddYearsText, <<"target before year zero raises badarg">>),
    true = contains(ParseText, <<"Explicit positive expanded years with a leading + are accepted; negative years raise badarg.">>),
    Codes = codes(Interval),
    [<<"/">>, <<"--">>] = Codes,
    true = contains(IntervalText, <<"omitted end-zone inheritance completes the end's raw local date/time before applying the start zone">>),
    io:format("expanded_year/0: ~ts~n", [YearText]),
    io:format("add_years/2: target-before-zero badarg present~n"),
    io:format("parse/1: explicit-positive-expanded accepted; negative badarg present~n"),
    io:format("parse_interval/1 separate code nodes: ~tp~n", [Codes]),
    io:format("parse_interval/1 surrounding inheritance prose: readable~n"),
    io:format("EXIT=0~n").

doc(Entries, Key) ->
    {{_, _, _}, _, _, Doc, _} = lists:keyfind(Key, 1, Entries),
    maps:get(<<"en">>, Doc).

contains(Text, Part) -> binary:match(Text, Part) =/= nomatch.

text(Value) when is_binary(Value) -> Value;
text(Value) when is_list(Value) -> iolist_to_binary([text(Item) || Item <- Value]);
text(Value) when is_map(Value) -> iolist_to_binary([text(Item) || Item <- maps:values(Value)]);
text(Value) when is_tuple(Value) -> iolist_to_binary([text(Item) || Item <- tuple_to_list(Value)]);
text(_) -> <<>>.

codes({code, _, Children}) -> [text(Children)];
codes(Value) when is_list(Value) -> lists:append([codes(Item) || Item <- Value]);
codes(Value) when is_map(Value) -> lists:append([codes(Item) || Item <- maps:values(Value)]);
codes(Value) when is_tuple(Value) -> lists:append([codes(Item) || Item <- tuple_to_list(Value)]);
codes(_) -> [].
