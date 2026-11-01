-module(iso8601_doc_tests).

-include_lib("eunit/include/eunit.hrl").

doc_examples_cover_public_api_test() ->
    Source = source_path(),
    {ok, Contents} = file:read_file(Source),
    Blocks = extract_blocks(binary_to_list(Contents)),
    Calls = lists:usort(lists:append([remote_calls(parse_block(Block)) || Block <- Blocks])),
    Exports = [
        Export
     || Export = {Name, _Arity} <- iso8601:module_info(exports), Name =/= module_info
    ],
    ?assert(Blocks =/= []),
    ?assertEqual(lists:sort(Exports), lists:sort(Calls)),
    lists:foreach(fun execute_block/1, Blocks).

wrong_literal_is_rejected_test() ->
    ?assertMatch(
        {error, _},
        try_block("{{1970, 1, 1}, {9, 0, 0}} = iso8601:add_time({0, 0, 0}, 1, 0, 0).")
    ).

malformed_example_is_rejected_test() ->
    ?assertMatch({error, _}, try_block("this is not Erlang.")).

invalid_duration_iodata_is_rejected_test() ->
    ?assertError(badarg, iso8601:parse_duration([256])),
    ?assertError(badarg, iso8601:apply_duration({{2017, 1, 1}, {0, 0, 0}}, [256])).

source_path() ->
    filename:join([code:lib_dir(iso8601), "src", "iso8601.erl"]).

extract_blocks(Source) ->
    extract_lines(string:split(Source, "\n", all), outside, [], []).

extract_lines([], outside, _Current, Blocks) ->
    lists:reverse(Blocks);
extract_lines([], inside, _Current, _Blocks) ->
    erlang:error(unclosed_documentation_example);
extract_lines([Line | Rest], State, Current, Blocks) ->
    Text = comment_text(Line),
    case {State, string:trim(Text)} of
        {outside, "```"} ->
            extract_lines(Rest, inside, [], Blocks);
        {outside, "'''"} ->
            erlang:error(unmatched_documentation_example_close);
        {inside, "```"} ->
            erlang:error(nested_documentation_example);
        {inside, "'''"} ->
            case Current of
                [] -> erlang:error(empty_documentation_example);
                _ -> ok
            end,
            Block = lists:flatten(lists:join("\n", lists:reverse(Current))) ++ "\n",
            extract_lines(Rest, outside, [], [Block | Blocks]);
        {inside, _} ->
            extract_lines(Rest, inside, [Text | Current], Blocks);
        {outside, _} ->
            extract_lines(Rest, outside, Current, Blocks)
    end.

comment_text(Line) ->
    case string:trim(Line, leading) of
        [$%, $% | Rest] -> strip_one_space(Rest);
        _ -> []
    end.

strip_one_space([$\s | Rest]) -> Rest;
strip_one_space(Rest) -> Rest.

parse_block(Block) ->
    case erl_scan:string(Block) of
        {ok, Tokens, _} -> parse_statements(Tokens, [], [], Block);
        {error, Reason, _} -> erlang:error({example_scan_error, Reason, Block})
    end.

parse_statements([], [], Exprs, _Block) ->
    lists:append(lists:reverse(Exprs));
parse_statements([], _Current, _Exprs, Block) ->
    erlang:error({example_missing_terminator, Block});
parse_statements([{dot, _} = Dot | Rest], Current, Exprs, Block) ->
    Statement = lists:reverse([Dot | Current]),
    case erl_parse:parse_exprs(Statement) of
        {ok, Parsed} -> parse_statements(Rest, [], [Parsed | Exprs], Block);
        {error, Reason} -> erlang:error({example_parse_error, Reason, Block})
    end;
parse_statements([Token | Rest], Current, Exprs, Block) ->
    parse_statements(Rest, [Token | Current], Exprs, Block).

execute_block(Block) ->
    case try_block(Block) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({example_evaluation_error, Reason, Block})
    end.

try_block(Block) ->
    try parse_block(Block) of
        Exprs ->
            try erl_eval:exprs(Exprs, erl_eval:new_bindings()) of
                {value, Value, _Bindings} -> {ok, Value}
            catch
                Class:Reason -> {error, {Class, Reason}}
            end
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

remote_calls(Exprs) ->
    lists:usort(lists:append([remote_calls_expr(Expr) || Expr <- Exprs])).

remote_calls_expr({call, _, {remote, _, {atom, _, iso8601}, {atom, _, Name}}, Args}) ->
    [{Name, length(Args)} | remote_calls_expr(Args)];
remote_calls_expr(Tuple) when is_tuple(Tuple) ->
    lists:append([remote_calls_expr(Element) || Element <- tuple_to_list(Tuple)]);
remote_calls_expr(List) when is_list(List) ->
    lists:append([remote_calls_expr(Element) || Element <- List]);
remote_calls_expr(_) ->
    [].
