#!/usr/bin/env escript
%%! -noshell
main([Before, After]) ->
    A = parse(Before), B = parse(After),
    Af = [norm(F) || F = {function,_,_,_,_} <- A], Bf = [norm(F) || F = {function,_,_,_,_} <- B],
    At = [norm(F) || F = {attribute,_,type,_} <- A], Bt = [norm(F) || F = {attribute,_,type,_} <- B],
    As = [norm(F) || F = {attribute,_,spec,_} <- A], Bs = [norm(F) || F = {attribute,_,spec,_} <- B],
    Ae = attrs(A, export), Be = attrs(B, export),
    Ate = attrs(A, export_type), Bte = attrs(B, export_type),
    90 = length(Af), true = Af =:= Bf,
    15 = length(Ae), true = Ae =:= Be,
    11 = length(Ate), true = Ate =:= Bte,
    16 = length(At), true = At =:= Bt,
    29 = length(As), true = As =:= Bs,
    io:format("function_forms=90 identical exports=15 identical exported_types=11 identical type_definitions=16 identical specs=29 identical~nEXIT=0~n").
parse(File) -> {ok, Forms} = epp:parse_file(File, [], []), Forms.
attrs(Forms, Name) -> lists:append([V || {attribute,_,N,V} <- Forms, N =:= Name]).
norm(Form) -> erl_parse:map_anno(fun(_) -> 0 end, Form).
