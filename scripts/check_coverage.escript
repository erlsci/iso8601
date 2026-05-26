#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/test/lib/iso8601/ebin

-define(THRESHOLD, 95).
-define(COVERDATA_FILES, [
    "_build/test/cover/eunit.coverdata",
    "_build/test/cover/ct.coverdata",
    "_build/test/cover/proper.coverdata"
]).
-define(MODULES, [
    iso8601
]).

-define(AMENDED, #{}).

main(_Args) ->
    lists:foreach(
        fun(F) ->
            case filelib:is_file(F) of
                true -> cover:import(F);
                false -> ok
            end
        end,
        ?COVERDATA_FILES
    ),
    {TotalCov, TotalUncov, Failed} = lists:foldl(
        fun(Mod, {CovAcc, UncovAcc, FailAcc}) ->
            case cover:analyse(Mod, calls, line) of
                {ok, Lines} ->
                    Cov = length([1 || {{_, _}, N} <- Lines, N > 0]),
                    Uncov = length([1 || {{_, _}, N} <- Lines, N =:= 0]),
                    Pct = case Cov + Uncov of
                        0 -> 100;
                        T -> Cov * 100 div T
                    end,
                    io:format("  ~-25s ~3w%  (~w/~w)~n", [Mod, Pct, Cov, Cov + Uncov]),
                    NewFail = case check_module(Mod, Pct) of
                        pass -> FailAcc;
                        {fail, Reason} -> [{Mod, Reason} | FailAcc]
                    end,
                    {CovAcc + Cov, UncovAcc + Uncov, NewFail};
                {error, _} ->
                    io:format("  ~-25s  (no data)~n", [Mod]),
                    {CovAcc, UncovAcc, [{Mod, "no data"} | FailAcc]}
            end
        end,
        {0, 0, []},
        ?MODULES
    ),
    Total = TotalCov + TotalUncov,
    Pct = case Total of
        0 -> 100;
        _ -> TotalCov * 100 div Total
    end,
    io:format("~n  Total: ~w% (~w/~w executable lines)~n", [Pct, TotalCov, Total]),
    io:format("  Threshold: ~w% (per-module)~n", [?THRESHOLD]),
    case {Failed, Pct >= ?THRESHOLD} of
        {[], true} ->
            io:format("  PASS~n"),
            halt(0);
        {[], false} ->
            io:format("  FAIL: aggregate ~w% < ~w%~n", [Pct, ?THRESHOLD]),
            halt(1);
        {_, _} ->
            lists:foreach(
                fun({Mod, Reason}) ->
                    io:format("  FAIL: ~s — ~s~n", [Mod, Reason])
                end,
                lists:reverse(Failed)
            ),
            halt(1)
    end.

check_module(Mod, Pct) ->
    case maps:find(Mod, ?AMENDED) of
        {ok, {Floor, _Lines}} ->
            case Pct >= Floor of
                true -> pass;
                false ->
                    {fail, lists:flatten(
                        io_lib:format("~w% < amended floor ~w%", [Pct, Floor]))}
            end;
        error ->
            case Pct >= ?THRESHOLD of
                true -> pass;
                false ->
                    {fail, lists:flatten(
                        io_lib:format("~w% < ~w%", [Pct, ?THRESHOLD]))}
            end
    end.
