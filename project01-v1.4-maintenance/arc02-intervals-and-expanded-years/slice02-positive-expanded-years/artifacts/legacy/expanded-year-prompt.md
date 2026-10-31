# CC brief — expanded year representation (1.4)

Add ISO 8601 *expanded representation* for years to iso8601, fix two related
parsing defects, and track it with a new issue. TDD, on a branch. Read `CLAUDE.md`
and `priv/ai/erlang/SKILL.md` (anti-patterns first) before coding.

## What expanded representation is

By mutual agreement, ISO 8601 lets a year carry an explicit leading sign (`+`/`-`)
and more than four digits — for years outside `0000`–`9999`, or to be explicit
about sign/era. Examples: `+0002007-01-01` (= 2007), `+10000-01-01` (year 10000),
`-0001-01-01` (year −1, astronomical numbering where year 0 = 1 BCE). A leading
sign is **required** for expanded form; an unsigned year stays exactly four digits.

## Step 1 — open a tracking issue

Re-confirm open issues with `gh issue list --repo erlsci/iso8601 --state open`
(reuse if one already covers this; none did at last check). Otherwise create:

> **Title:** Support ISO 8601 expanded year representation (`±Y…Y`)
>
> **Body:** The parser hard-codes a 4-character year, so signed/expanded years
> aren't supported and misbehave: `parse("+2007-01-01")` misreads the year via
> `list_to_integer("+200")` before failing, and unsigned overlength years like
> `"10000-01-01"` fail with an internal `badarg` rather than parsing as a 5-digit
> year. Add expanded representation (sign + agreed digit count), including years
> beyond 9999 and (pending the type decision below) negative astronomical years.

## Step 2 — branch & TDD

Branch `task/expanded-year` off `main`. Each item: write the failing test, see it
fail, implement, see it pass.

### Defects to lock with tests (currently misbehave)

```erlang
expanded_year_test_() ->
    [%% explicit + sign, leading zeros
     ?_assertEqual({{2007,1,1},{0,0,0}}, iso8601:parse("+0002007-01-01")),
     %% the old misparse: +2007 must be 2007, not 200
     ?_assertEqual({{2007,1,1},{0,0,0}}, iso8601:parse("+2007-01-01")),
     %% years beyond 9999 (positive — no type change needed)
     ?_assertEqual({{10000,1,1},{0,0,0}}, iso8601:parse("+10000-01-01")),
     %% unsigned overlength is NOT expanded (no sign) → clean badarg
     ?_assertError(badarg, iso8601:parse("10000-01-01")),
     %% empty after sign → badarg
     ?_assertError(badarg, iso8601:parse("+-01-01"))].
```

### Negative / astronomical years (gated on the type decision in Step 3)

```erlang
expanded_year_negative_test_() ->
    [?_assertEqual({{-1,1,1},{0,0,0}}, iso8601:parse("-0001-01-01")),
     ?_assertEqual({{0,1,1},{0,0,0}},  iso8601:parse("-0000-01-01")),
     ?_assertEqual({{-1,1,1},{0,0,0}}, iso8601:parse_exact("-0001-01-01"))].
```

## Step 3 — implementation

Add sign-detecting clauses to `year/2` and consume the full run of digits as the
year; reuse `month_or_week/2` for the remainder. Sketch:

```erlang
year([$+ | Rest], Acc) -> expanded_year(1, Rest, Acc);
year([$- | Rest], Acc) -> expanded_year(-1, Rest, Acc);
year([Y1, Y2, Y3, Y4 | Rest], Acc) ->
    acc([Y1, Y2, Y3, Y4], Rest, year, Acc, fun month_or_week/2);
year(_, _) ->
    error(badarg).

expanded_year(Sign, Str, Acc) ->
    {Digits, Rest} = lists:splitwith(fun(C) -> C >= $0 andalso C =< $9 end, Str),
    case Digits of
        [] -> error(badarg);
        _  -> month_or_week(Rest, [{year, Sign * list_to_integer(Digits)} | Acc])
    end.
```

Notes:
- Unsigned years stay exactly 4 digits (existing clause unchanged), so
  `"10000-01-01"` still errors cleanly — confirm it lands as `badarg`.
- `"+2007"` with no month/day → `month_or_week("")` → defaults to `{2007,1,1}`,
  matching the existing date-defaulting behavior.
- Basic-format expanded (`+20070101`, no separators) is **out of scope** — the
  digit run would swallow month/day. Document it; don't try to support it in 1.4.

### The type decision (this is the backward-compat call — confirm with Duncan)

- **Positive expanded years need no type change.** `year() :: non_neg_integer()`
  already permits values > 9999, and `parse/1`'s `calendar:datetime()` return spec
  is fine for them. Implement these unconditionally.
- **Negative years require widening exported types:** `year()` from
  `non_neg_integer()` to `integer()`, and changing the return spec of `parse/1`
  and `parse_exact/1` from `calendar:datetime()` to the library's own `datetime()`
  (and ensure `datetime()` uses the library `year()`/`integer()` year rather than
  `calendar:date()`, whose year is non-negative). These are *widenings* (supersets,
  low real-world risk) but they touch the public type surface, so they're a
  deliberate 1.4 decision.

Implement negative-year support **only if Duncan has approved the type widening.**
If not, ship positive expanded years now and leave the negative tests/`year()`
change for a follow-up. Report which path you took.

## Step 4 — verify

- `rebar3 compile` clean (no warnings), `xref` clean, `dialyzer` clean (especially
  after any `year()`/`datetime()`/spec change — dialyzer is the check that the
  widened types are consistent).
- `rebar3 eunit -v` green; `rebar3 proper -c` non-zero property count; coverage
  still **100%** (new `year/2` and `expanded_year/3` clauses are reached by the
  new tests). Consider a PropEr property: `parse(format(DT)) =:= DT` already
  exists for 4-digit years; if negative years are in, extend the generator's year
  range and confirm `format/1` round-trips them (it may need a sign-emitting
  branch — if so, that's part of this work).
- Backward-compat: existing exports/specs unchanged **except** the deliberately
  widened `year()`/return specs (if negative years are included); list exactly
  what changed in the closing report.

## Step 5 — PR

`gh pr create` against `main`, `Closes #<new issue>`. Summarize: expanded
representation support, the two defect fixes, and (if included) the `year()`
widening for negative years with its backward-compat rationale. Leave `vsn` at
1.3.4 (Duncan bumps to 1.4 at release). Per-criterion closing disposition; name
anything not verified by running.
