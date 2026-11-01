#!/usr/bin/env escript
%%! -noshell

main([Baseline, Current]) ->
    BaseForms = parse(Baseline),
    CurrentForms = parse(Current),
    BaseFunctions = [normalize(Form) || Form = {function, _, _, _, _} <- BaseForms],
    CurrentFunctions = [normalize(Form) || Form = {function, _, _, _, _} <- CurrentForms],
    BaseExports = attributes(BaseForms, export),
    CurrentExports = attributes(CurrentForms, export),
    BaseTypeExports = attributes(BaseForms, export_type),
    CurrentTypeExports = attributes(CurrentForms, export_type),
    BaseTypes = [normalize(Form) || Form = {attribute, _, type, _} <- BaseForms],
    CurrentTypes = [normalize(Form) || Form = {attribute, _, type, _} <- CurrentForms],
    BaseSpecs = [Form || Form = {attribute, _, spec, _} <- BaseForms],
    CurrentSpecs = [Form || Form = {attribute, _, spec, _} <- CurrentForms],
    Allowed = [{add_time, 4}, {subtract_time, 4}, {parse_exact, 1},
        {parse_duration, 1}, {apply_duration, 2}],
    BaseUnchangedSpecs = [normalize(F) || F <- BaseSpecs, not lists:member(spec_id(F), Allowed)],
    CurrentUnchangedSpecs = [normalize(F) || F <- CurrentSpecs, not lists:member(spec_id(F), Allowed)],
    true = length(BaseFunctions) =:= 90,
    true = BaseFunctions =:= CurrentFunctions,
    true = BaseExports =:= CurrentExports,
    true = length(CurrentExports) =:= 15,
    true = BaseTypeExports =:= CurrentTypeExports,
    true = length(CurrentTypeExports) =:= 11,
    true = length(BaseTypes) =:= 16,
    true = BaseTypes =:= CurrentTypes,
    true = length(BaseSpecs) =:= 29,
    true = length(CurrentSpecs) =:= 29,
    true = BaseUnchangedSpecs =:= CurrentUnchangedSpecs,
    true = lists:sort([spec_id(F) || F <- CurrentSpecs, lists:member(spec_id(F), Allowed)]) =:=
        lists:sort(Allowed),
    lists:foreach(fun(F) -> io:format("spec ~tp: ~ts~n", [spec_id(F), compact(erl_pp:form(F))]) end,
        [F || F <- CurrentSpecs, lists:member(spec_id(F), Allowed)]),
    io:format("runtime_function_forms=90 identical exports=15 identical type_exports=11 identical type_definitions=16 identical specs=29 five corrected others unchanged~n").

parse(File) ->
    case epp:parse_file(File, [], []) of
        {ok, Forms} -> Forms;
        Error -> erlang:error({epp, File, Error})
    end.

attributes(Forms, Name) ->
    lists:append([Value || {attribute, _, Name0, Value} <- Forms, Name0 =:= Name]).

spec_id({attribute, _, spec, {{Name, Arity}, _}}) -> {Name, Arity}.

normalize(Form) ->
    erl_parse:map_anno(fun(_) -> 0 end, Form).

compact(Text) ->
    re:replace(Text, "\\s+", "", [global, {return, list}]).
