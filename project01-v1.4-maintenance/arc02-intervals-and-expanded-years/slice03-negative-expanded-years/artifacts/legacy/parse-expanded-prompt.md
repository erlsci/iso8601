# CC brief — expanded/negative-year parsing via opt-in `parse_expanded/1` (1.4)

Add support for ISO 8601 expanded representation **including negative
(astronomical) years** — for paleontological/geological timescales — through a
new, opt-in function whose own wide types keep every existing contract untouched.
TDD, on a branch, behind a new issue. Read `CLAUDE.md` and
`priv/ai/erlang/SKILL.md` (anti-patterns first) before coding.

## Design (already decided — don't redesign)

The opt-in is a **distinctly-named function with its own wide types**, NOT a
`parse/2` options bag and NOT a record (house rule: no records across module
boundaries; and the library already expresses variants by name — `parse_exact` —
not options).

```erlang
-export([parse_expanded/1, format_expanded/1]).
-export_type([expanded_year/0, expanded_datetime/0]).

-type expanded_year() :: integer().
-type expanded_datetime() ::
        {{expanded_year(), month(), day()}, {hour(), minute(), second() | float()}}.

-spec parse_expanded(iodata()) -> expanded_datetime().
-spec format_expanded(expanded_datetime()) -> binary().
```

Why this shape: the wide (integer-year) return lives only on the new function and
new types, so `year()`, `datetime()`, and the `calendar:datetime()` return specs
of `parse/1` and `parse_exact/1` are **unchanged**. Callers chaining `parse/1`
output into `calendar:*` keep their dialyzer guarantee; only `parse_expanded`
users take on the wider (non-calendar-compatible) type. Purely additive.

## Hard constraints

- **Additive / non-breaking.** No change to `year()` (stays `non_neg_integer()`),
  `datetime()`, or the specs/behavior of `parse/1`, `parse_exact/1`, `format/1`,
  `parse_interval/1`. Verify with a diff: existing exports/specs/types only gain
  new entries.
- **OTP 20+ floor.** No post-20 constructs. No `-doc` — use edoc `%% @doc`.
- **100% coverage** stays green; full gate clean (compile/xref/dialyzer/eunit/proper).
- You implement and self-assess; a reviewer verifies. Write to the floor.

## Step 1 — new issue

`gh issue list --repo erlsci/iso8601 --state open` first (reuse if one already
fits). Otherwise create:

> **Title:** Support negative / astronomical years (expanded representation) for scientific timescales
>
> **Body:** Issue #70 / PR #71 added *positive* expanded years. Negative
> (astronomical, BCE) years — needed for paleontological and geological use — were
> deferred because emitting them would force `parse/1`'s `calendar:datetime()`
> return contract to widen. This adds an opt-in `parse_expanded/1` / `format_expanded/1`
> with their own integer-year types (`expanded_datetime/0`), leaving `parse/1`'s
> calendar-compatible contract intact.

## Step 2 — branch

Branch off whichever ref has the positive-expanded work: `main` if PR #71 is
merged, otherwise `task/expanded-year`. Call it `task/expanded-year-negative`.

## Step 3 — implementation

### Thread the sign decision so only `parse_expanded` accepts `-`

Refactor the year entry to carry an allow-negative flag. `parse/1` and
`parse_exact/1` pass `positive_only` (so a leading `-` is a clean `badarg` — this
also *tightens* today's incidental `list_to_integer` crash into an intentional
reject); `parse_expanded/1` passes `allow_neg`:

```erlang
parse(Str)          -> ... year(Str, [], positive_only) ...   %% trunc seconds (unchanged)
parse_exact(Str)    -> ... year(Str, [], positive_only) ...   %% fractional (unchanged)
parse_expanded(Str) -> ... year(Str, [], allow_neg) ...       %% fractional + wide type

year([$- | Rest], Acc, allow_neg)     -> expanded_year(-1, Rest, Acc);
year([$- | _],    _,   positive_only) -> error(badarg);
year([$+ | Rest], Acc, _)             -> expanded_year(1, Rest, Acc);
year([Y1,Y2,Y3,Y4 | Rest], Acc, _)    -> acc([Y1,Y2,Y3,Y4], Rest, year, Acc, fun month_or_week/2);
year(_, _, _)                         -> error(badarg).
```

`parse_expanded/1` uses the **exact** (fractional-second-preserving) path like
`parse_exact/1` — it's the information-preserving choice for precision-sensitive
users, and avoids a `parse_exact_expanded` name explosion. Add the
`is_binary/1` → `binary_to_list` clause like the other parsers.

Note: interval endpoints (`parse_endpoint/1`) keep using `parse`/`parse_exact`
(`positive_only`) — negative years in intervals are out of scope here; document
that boundary.

### `format_expanded/1`

Inverse of `parse_expanded/1`. Emit an **explicit sign** and a zero-padded
(minimum 4-digit) year, then the usual `-MM-DDTHH:MM:SSZ` tail (reuse `format/1`'s
tail logic; only the year portion differs). `+` for years ≥ 0, `-` for < 0, e.g.
`-0001-01-01T00:00:00Z`, `+10000-01-01T00:00:00Z`. This is distinct from
`format/1` (which stays calendar-style and unsigned for years ≤ 9999).

### Cleanup (folded in per prior review)

In `extract_date_part/1`, replace `string:tokens(Str, "T")` with
`string:split(Str, "T")` for consistency with the rest of the interval code
(behavior is identical — the start is pre-validated — so no test change needed,
but confirm the interval tests stay green).

## Step 4 — tests (TDD: red → green per item)

```erlang
parse_expanded_test_() ->
    [?_assertEqual({{-1,1,1},{0,0,0}},     iso8601:parse_expanded("-0001-01-01")),
     ?_assertEqual({{0,1,1},{0,0,0}},      iso8601:parse_expanded("-0000-01-01")),
     ?_assertEqual({{-44,3,15},{0,0,0}},   iso8601:parse_expanded("-0044-03-15")),  %% Ides of March, 44 BCE (astro -44)
     ?_assertEqual({{2007,1,1},{0,0,0}},   iso8601:parse_expanded("+0002007-01-01")),
     ?_assertEqual({{10000,1,1},{0,0,0}},  iso8601:parse_expanded("+10000-01-01")),
     ?_assertEqual({{2007,1,1},{0,0,0}},   iso8601:parse_expanded("2007-01-01")),   %% plain years work too
     ?_assertError(badarg,                 iso8601:parse_expanded("+-01-01"))].

%% the calendar-compatible parsers must REJECT negatives (contract intact)
parse_rejects_negative_test_() ->
    [?_assertError(badarg, iso8601:parse("-0001-01-01")),
     ?_assertError(badarg, iso8601:parse_exact("-0001-01-01"))].

format_expanded_test_() ->
    [?_assertEqual(<<"-0001-01-01T00:00:00Z">>, iso8601:format_expanded({{-1,1,1},{0,0,0}})),
     ?_assertEqual(<<"+10000-01-01T00:00:00Z">>, iso8601:format_expanded({{10000,1,1},{0,0,0}}))].
```

PropEr (add to `test/prop_iso8601.erl`): round-trip over negative and large years —

```erlang
prop_expanded_roundtrip() ->
    ?FORALL({Y, Mo, D, H, Mn, S},
            {range(-9999, 9999), range(1,12), range(1,28),
             range(0,23), range(0,59), range(0,59)},
        begin
            DT = {{Y, Mo, D}, {H, Mn, S}},
            DT =:= iso8601:parse_expanded(
                     binary_to_list(iso8601:format_expanded(DT)))
        end).
```

## Step 5 — verify & PR

- `rebar3 compile` clean (no warnings), `xref` clean, `dialyzer` clean (it's the
  check that `expanded_datetime()` is internally consistent and that `parse/1`'s
  contract didn't move).
- `rebar3 eunit -v` green; `rebar3 proper -c` non-zero property count; coverage
  **100%** (new `year/3` clauses, `parse_expanded`, `format_expanded` all reached).
- Backward-compat diff: `git diff <base> -- src/iso8601.erl | grep -E '^\-.*(-export|-spec|-type)'`
  shows only additions; `year()`/`datetime()`/`parse`/`parse_exact`/`format` specs
  unchanged.
- `gh pr create` against `main`, `Closes #<new issue>`. Summarize: opt-in
  `parse_expanded/1` + `format_expanded/1` with `expanded_datetime/0`; negative
  years supported without touching `parse/1`'s calendar-compatible contract; the
  `parse`/`parse_exact` negative-reject tightening; the `extract_date_part`
  cleanup. `vsn` stays 1.3.4 (Duncan bumps to 1.4 at release). Per-criterion
  closing disposition; name anything not verified by running.
```
