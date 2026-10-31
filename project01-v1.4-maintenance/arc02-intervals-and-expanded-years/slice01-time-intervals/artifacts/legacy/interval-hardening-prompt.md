# CC brief — harden interval parsing (edge cases & input validation)

Follow-up to the interval feature (commit `c77e9d9` on `task/time-intervals`).
Reviewer found edge cases and input-validation gaps. Close them TDD-style; fold
into the same branch / interval PR. Read `CLAUDE.md` and `priv/ai/erlang/SKILL.md`
(anti-patterns first) before coding.

## Constraints

- **Non-breaking, additive.** No change to the public surface or to the four
  supported interval forms' existing behavior. Only tightening of error handling
  and added tests.
- **OTP 20+ floor.** `string:split/3` (the new-`string`-module function) is OTP
  20+ and fine; prefer it over `string:tokens/2` where empty-segment handling
  matters (see G3). No post-20 constructs.
- **100% coverage stays green**; full gate clean (compile/xref/dialyzer/eunit/
  proper). Every new clause needs a test that reaches it.
- You implement and self-assess; a reviewer verifies. Write to the floor.

For each gap: write the failing/locking test first, see it fail (or, for the
"lock current behavior" cases, see it pass and assert it's intentional), then make
the change, then confirm green.

## G1 — Canonical minute-precision examples aren't tested

The implementation was tested with second-precision `…:00Z` inputs, not the
literal minute-precision strings from the ISO 8601 spec / issue #3. Add tests for
the exact canonical forms and confirm they resolve correctly:

```erlang
?_assertEqual({{2007,12,14},{15,30,0}},
    element(2, iso8601:interval_bounds(
        iso8601:parse_interval("2007-12-14T13:30/15:30")))),
?_assertEqual({{2007,11,15},{17,0,0}},
    element(2, iso8601:interval_bounds(
        iso8601:parse_interval("2007-11-13T09:00/15T17:00")))).
```

If `parse/1` already accepts minute precision (it likely defaults missing seconds
to 0), these pass and we've locked the spec examples. If it does **not**, decide
explicitly: either make it work, or document in the README that interval endpoints
require second precision — don't leave it implied.

## G2 — Malformed end fragments crash with `function_clause` instead of `badarg`

`classify_end_fragment/1`'s `T` branch does `string:tokens(Str, "T")` and only
matches 1- or 2-element results, so an end with two `T`s (e.g. `"15T17T00"`) falls
through to a `function_clause` — inconsistent with the library's `badarg` contract
for malformed input. Also, `string:tokens` collapses empties, so `"15TT00"` would
mis-parse rather than error.

Fix: classify the `T`-bearing fragment with `string:split(Str, "T", all)` and match
exactly `[DayPart, TimePart]` / `[TimePart]`; anything else → `error(badarg)`. Add
tests:

```erlang
?_assertError(badarg, iso8601:parse_interval("2007-11-13/15T17T00")),
?_assertError(badarg, iso8601:parse_interval("2007-11-13/15TT00")).
```

## G3 — Empty interval sides parse instead of erroring

`split_interval/1` uses `string:tokens(Str, "/")`, which drops empty tokens, so
`"A//B"` is read as a two-part `A/B`, and `"/B"` / `"A/"` collapse to a single
token. A datetime or duration never contains `/`, so the rule is "exactly one `/`,
both sides non-empty."

Fix: split with `string:split(Str, "/", all)` and require exactly two non-empty
parts; otherwise fall through to the `--` path (zero `/`) or `error(badarg)`
(>1 `/`, or an empty side). Apply the same non-empty check to the `--` split. Tests:

```erlang
?_assertError(badarg, iso8601:parse_interval("2007//2008")),
?_assertError(badarg, iso8601:parse_interval("/2008-05-11T15:30:00Z")),
?_assertError(badarg, iso8601:parse_interval("2007-03-01T13:00:00Z/")),
?_assertError(badarg, iso8601:parse_interval("2007----2008")).
```

## G4 — Inheritance silently assumes extended-format starts

`extract_year_month/1` (`lists:sublist(Str, 7)`) and `extract_year/1`
(`lists:sublist(Str, 4)`) only yield `"YYYY-MM"` / `"YYYY"` for **extended**
(dashed) starts. A basic-format start (e.g. `"20071113"`) with an abbreviated end
would slice wrong and produce a confusing failure.

Fix: when resolving an abbreviated end, require the start to be extended format
(contains `-`); if not, `error(badarg)` with the limitation documented (basic-format
inheritance is out of scope). Add a locking test and a one-line note in the README
interval section:

```erlang
?_assertError(badarg, iso8601:parse_interval("20071113/15")).
```

## G5 — Month-day-with-time abbreviated end

An abbreviated end like `"03-14T10:00"` (month-day + time, inheriting year) is not
a handled shape: the `day_and_time` branch treats the pre-`T` part as a bare day,
builds a malformed string, and fails. It currently fails *safe* (to a parse error),
but lock that as an explicit `badarg` and document it as an unsupported abbreviation
in 1.4 (the supported abbreviated-end shapes are: time-only, day-only, month-day,
and day+time where the day is a bare `DD`).

```erlang
?_assertError(badarg, iso8601:parse_interval("2008-02-15/03-14T10:00")).
```

(If you find it's cheap to support properly instead of rejecting, that's fine too —
but a clean `badarg` + doc note is acceptable for 1.4.)

## Verify

- `rebar3 compile` clean (no warnings), `xref` clean (confirm no deprecated-call
  warnings from the `string` functions on the OTP 24–27 matrix), `dialyzer` clean.
- `rebar3 eunit -v` green; `rebar3 proper -c` reports a non-zero property count;
  coverage still **100%** (new error clauses are reached by the new tests).
- Backward-compat: the four valid interval forms and all existing tests still pass
  unchanged; public exports/specs/types unchanged.

Fold these commits into the interval PR (`Closes #3`). Close with a per-gap
disposition (G1–G5: fixed / locked-as-badarg + documented), naming anything not
verified by running.
