#!/usr/bin/env escript
%%! -noshell

main([Source, Chunk]) ->
    {ok, Forms} = epp:parse_file(Source, [], []),
    Exports = lists:append([Names || {attribute, _, export, Names} <- Forms]),
    TypeExports = lists:append([Names || {attribute, _, export_type, Names} <- Forms]),
    {ok, ChunkBytes} = file:read_file(Chunk),
    {docs_v1, _, _, _, ModuleDoc, _, Entries} = binary_to_term(ChunkBytes),
    FunctionEntries = [
        {{function, Name, Arity}, Anno, Signatures, Doc, Meta}
     || {{function, Name, Arity}, Anno, Signatures, Doc, Meta} <- Entries
    ],
    TypeEntries = [
        {{type, Name, Arity}, Anno, Signatures, Doc, Meta}
     || {{type, Name, Arity}, Anno, Signatures, Doc, Meta} <- Entries
    ],
    MissingFunctions = [
        {Name, Arity}
     || {Name, Arity} <- Exports,
        Name =/= module_info,
        not has_english_doc(FunctionEntries, Name, Arity)
    ],
    MissingExamples = [
        {Name, Arity}
     || {Name, Arity} <- Exports,
        Name =/= module_info,
        count_examples(FunctionEntries, Name, Arity) =:= 0
    ],
    MissingTypes = [
        {Name, Arity}
     || {Name, Arity} <- TypeExports,
        not has_english_type_doc(TypeEntries, Name, Arity)
    ],
    FunctionExamples = [
        {{Name, Arity}, count_examples(FunctionEntries, Name, Arity)}
     || {Name, Arity} <- Exports,
        Name =/= module_info
    ],
    ModuleCount = case maps:get(<<"en">>, ModuleDoc, []) of [] -> 0; _ -> 1 end,
    io:format("module_overview=~B~n", [ModuleCount]),
    io:format("public_exports=~B~n", [length([E || E = {N, _} <- Exports, N =/= module_info])]),
    io:format("function_docs=~B~n", [length([E || E = {N, A} <- Exports, N =/= module_info,
        has_english_doc(FunctionEntries, N, A)])]),
    io:format("function_examples=~tp~n", [FunctionExamples]),
    io:format("missing_function_docs=~tp~n", [MissingFunctions]),
    io:format("missing_function_examples=~tp~n", [MissingExamples]),
    io:format("exported_types=~B~n", [length(TypeExports)]),
    io:format("type_docs=~B~n", [length([E || E = {N, A} <- TypeExports,
        has_english_type_doc(TypeEntries, N, A)])]),
    io:format("missing_type_docs=~tp~n", [MissingTypes]),
    case ModuleCount =:= 1 andalso MissingFunctions =:= [] andalso
        MissingExamples =:= [] andalso MissingTypes =:= [] of
        true -> halt(0);
        false -> halt(1)
    end.

has_english_doc(Entries, Name, Arity) ->
    case lists:keyfind({function, Name, Arity}, 1, Entries) of
        {{function, _, _}, _, _, Doc, _} -> maps:get(<<"en">>, Doc, []) =/= [];
        false -> false
    end.

has_english_type_doc(Entries, Name, Arity) ->
    case lists:keyfind({type, Name, Arity}, 1, Entries) of
        {{type, _, _}, _, _, Doc, _} -> maps:get(<<"en">>, Doc, []) =/= [];
        false -> false
    end.

count_examples(Entries, Name, Arity) ->
    case lists:keyfind({function, Name, Arity}, 1, Entries) of
        {{function, _, _}, _, _, Doc, _} -> count_pre(maps:get(<<"en">>, Doc, []));
        false -> 0
    end.

count_pre({pre, _, _}) -> 1;
count_pre(Tuple) when is_tuple(Tuple) ->
    lists:sum([count_pre(Item) || Item <- tuple_to_list(Tuple)]);
count_pre(List) when is_list(List) ->
    lists:sum([count_pre(Item) || Item <- List]);
count_pre(Map) when is_map(Map) ->
    lists:sum([count_pre(Item) || Item <- maps:values(Map)]);
count_pre(_) ->
    0.
