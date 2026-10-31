# CC brief — implement ISO 8601 time intervals (issue #3, 1.4)

Implement the full interval feature per `workbench/0003-time-intervals-design.md`
(read it first — this brief assumes its §1–§11). TDD, on a branch, behind issue #3.
Read `CLAUDE.md` and `priv/ai/erlang/SKILL.md` (anti-patterns first) before coding.

## Constraints

- **Additive / non-breaking.** New functions + new exported types only. Do not
  touch any existing export, `-spec`, type, or return shape. Verify with a diff at
  the end (existing public surface byte-for-byte unchanged).
- **OTP 20+ floor.** No post-OTP-20 BIFs/syntax. No `-doc`/EEP-48 — use edoc
  `%% @doc`. Spec every new exported function.
- **Do not bump `vsn`.** Duncan bumps `1.3.4 → 1.4` at release.
- **100% line coverage** stays green; full gate clean (compile/xref/dialyzer/
  eunit/proper). Treat warnings as defects.
- You implement and self-assess; a separate reviewer verifies. Write to the floor.

## Branch & issue

Branch `task/time-intervals` off `main` (CI fires on `task/**`). The PR `Closes #3`.

## Public API to build

```erlang
-export([parse_interval/1, interval_bounds/1, format_interval/1]).
-export_type([interval/0, duration/0]).

-type duration() :: datetime_plist().   %% the existing parse_duration/1 shape
-type interval() ::
        {interval, start_end,      datetime(), datetime()}
      | {interval, start_duration, datetime(), duration()}
      | {interval, duration_end,   duration(), datetime()}
      | {interval, duration,       duration()}.

-spec parse_interval(iodata()) -> interval().
-spec interval_bounds(interval()) -> {datetime(), datetime()}.
-spec format_interval(interval()) -> binary().
```

`datetime_plist()` stays private; `duration/0` is the exported alias.

## Build order (TDD — each step: write failing test → see it fail → implement → see it pass)

### Phase A — full endpoints, all four forms, both separators, bounds, format, signed durations

**A1 — separator split & component discrimination.**
Split the input on `/`; if there's no `/`, split on `--`. A side is a *duration*
iff (after stripping a leading `+`/`-`) it begins with `P`; otherwise it's a
*datetime* (parse with `parse/1`, or `parse_exact/1` when a fractional second is
present). Map to the tagged tuple:

| Input shape | Result |
|---|---|
| datetime `/` datetime | `{interval, start_end, S, E}` |
| datetime `/` duration | `{interval, start_duration, S, D}` |
| duration `/` datetime | `{interval, duration_end, D, E}` |
| duration (no separator) | `{interval, duration, D}` |

Reject (`error(badarg)`) anything with >1 separator, two durations, or empty sides.

Red tests (one generator per form, plus `--` variants):
```erlang
parse_interval_forms_test_() ->
    [?_assertMatch({interval, start_end, {{2007,3,1},{13,0,0}}, {{2008,5,11},{15,30,0}}},
        iso8601:parse_interval("2007-03-01T13:00:00Z/2008-05-11T15:30:00Z")),
     ?_assertMatch({interval, start_duration, {{2007,3,1},{13,0,0}}, [_|_]},
        iso8601:parse_interval("2007-03-01T13:00:00Z/P1Y2M10DT2H30M")),
     ?_assertMatch({interval, duration_end, [_|_], {{2008,5,11},{15,30,0}}},
        iso8601:parse_interval("P1Y2M10DT2H30M/2008-05-11T15:30:00Z")),
     ?_assertMatch({interval, duration, [_|_]},
        iso8601:parse_interval("P1Y2M10DT2H30M")),
     %% double-hyphen separator
     ?_assertMatch({interval, start_end, _, _},
        iso8601:parse_interval("2007-03-01T13:00:00Z--2008-05-11T15:30:00Z")),
     %% errors
     ?_assertError(badarg, iso8601:parse_interval("P1Y/P2Y")),
     ?_assertError(badarg, iso8601:parse_interval("2007/2008/2009"))].
```
(Confirm the `Z`-normalized datetimes by running — adjust expected tuples to what
`parse/1` actually returns.)

**A2 — `interval_bounds/1`.**
Resolve to concrete `{Start, End}`:
- `start_end` → `{S, E}`.
- `start_duration` → `{S, S + D}`.
- `duration_end` → `{E − D, E}`.
- `duration` → `error(badarg)` (no anchor).

This needs applying an *already-parsed* duration plist (forward and backward).
**Refactor, don't duplicate:** factor the core of `apply_duration/2` into an
internal `apply_signed_duration(Datetime, DurationPlist, Direction)` where
`Direction` is `forward | backward`; `forward` honors the plist's own sign,
`backward` inverts it (reuse the F-1 negate-closure idea — `Negate = (sign == "-")
xor (Direction == backward)`). Re-point `apply_duration/2` at it so its public
behavior is unchanged (confirm existing duration tests still pass).

Red tests:
```erlang
interval_bounds_test_() ->
    [?_assertEqual({{{2007,3,1},{13,0,0}}, {{2008,5,11},{15,30,0}}},
        iso8601:interval_bounds(iso8601:parse_interval(
            "2007-03-01T13:00:00Z/2008-05-11T15:30:00Z"))),
     %% start + duration == bounds' end (cross-check vs apply_duration)
     ?_assertEqual(iso8601:apply_duration({{2007,3,1},{13,0,0}}, "P1Y2M10DT2H30M"),
        element(2, iso8601:interval_bounds(iso8601:parse_interval(
            "2007-03-01T13:00:00Z/P1Y2M10DT2H30M")))),
     %% duration/end: start = end - duration
     ?_assertEqual({{2007,2,28},{12,0,0}},
        element(1, iso8601:interval_bounds(iso8601:parse_interval(
            "P1YT0H/2008-02-28T12:00:00Z")))),   %% verify expected by running
     ?_assertError(badarg,
        iso8601:interval_bounds(iso8601:parse_interval("P1Y")))].
```

**A3 — signed durations.** `parse_duration/1` already carries the sign; ensure the
`start_duration`/`duration_end` paths apply it correctly (e.g. `.../-P1Y` subtracts
in `start_duration`). Add a test with a signed duration in each duration-bearing form.

**A4 — `format_interval/1`.** Canonical (full, non-abbreviated) output as a binary,
`/` separator:
- `start_end` → `<<S/binary, "/", E/binary>>` using `format/1` on each.
- `start_duration` → `format/1(S) ++ "/" ++ format_duration(D)`.
- `duration_end` → `format_duration(D) ++ "/" ++ format/1(E)`.
- `duration` → `format_duration(D)`.

You'll need an internal `format_duration/1` (inverse of `parse_duration/1`):
build `"[-]P[nY][nM][nD][T[nH][nM][nS]]"` from the plist, honoring the sign and
omitting zero components (but always emit `P`, and emit `T` only if any time
component is non-zero). Keep it private unless trivial to expose.

Red test — round-trip on canonical intervals:
```erlang
format_interval_roundtrip_test_() ->
    Strs = ["2007-03-01T13:00:00Z/2008-05-11T15:30:00Z",
            "2007-03-01T13:00:00Z/P1Y2M10DT2H30M",
            "P1Y2M10DT2H30M/2008-05-11T15:30:00Z",
            "P1Y2M10DT2H30M"],
    [?_assertEqual(parse_interval_norm(S),
                   iso8601:parse_interval(
                     binary_to_list(iso8601:format_interval(
                       iso8601:parse_interval(S))))) || S <- Strs].
%% parse_interval_norm = iso8601:parse_interval; the point is
%% parse(format(parse(S))) =:= parse(S).
```

### Phase B — end-element inheritance (start/end only)

Gate this behind a green Phase A. Apply only when the END side of a `start_end`
interval is abbreviated (omits leading components). Algorithm (design §5.1):

1. Parse start fully; keep its raw token string and timezone designator.
2. Classify the end fragment to find its highest-order component:
   - has `T`, or pure time (`:` present, no `-`) → time-led → inherit the date
     (and year+month if it begins `DDThh`);
   - `MM-DD` (a `-` in date position, no year) → inherit year;
   - bare day-of-month with a full-date start → inherit year + month.
3. Textually prepend the inherited components, append start's tz if the end omits
   one, then `parse/1` the completed end string.
4. Genuinely ambiguous fragment → `error(badarg)` (don't guess).

Red tests — the four canonical cases (assert the resolved end equals the explicit
parse):
```erlang
interval_inheritance_test_() ->
    Eq = fun(Abbrev, Full) ->
        {interval, start_end, _, E} = iso8601:parse_interval(Abbrev),
        ?_assertEqual(iso8601:parse(Full), E)
    end,
    [Eq("2007-12-14T13:30/15:30",      "2007-12-14T15:30"),
     Eq("2008-02-15/03-14",            "2008-03-14"),
     Eq("2007-11-13/15",               "2007-11-15"),
     Eq("2007-11-13T09:00/15T17:00",   "2007-11-15T17:00"),
     %% ambiguous → badarg
     ?_assertError(badarg, iso8601:parse_interval("2007-11-13/9"))]  %% confirm this is the ambiguous case you settle on
    ].
```

## Errors

All malformed input → `error(badarg)`, matching `parse/1`/`parse_duration/1`. No
`{error, _}` tuples. `interval_bounds/1` on a duration-only interval → `badarg`.

## PropEr

Add to `test/prop_iso8601.erl`:
- `prop_interval_roundtrip`: generate a canonical interval (random datetime(s) +
  duration plist), `format_interval` it, `parse_interval` back, assert equal.
- `prop_start_duration_bounds`: for a generated start + duration,
  `element(2, interval_bounds({interval, start_duration, S, D}))` equals
  `apply_signed_duration(S, D, forward)`.
Reuse the bounded generators already in that file (years 1–9999, day 1–28, etc.).

## Verify before PR

- `rebar3 compile` clean (no warnings), `xref` clean, `dialyzer` clean.
- `rebar3 eunit -v` green; `rebar3 proper -c` reports a non-zero property count;
  coverage still **100%**.
- Backward-compat: `git diff main -- src/iso8601.erl | grep -E '^\-.*(-export|-spec|-type)'`
  shows only *additions* (new exports/specs/types), no removed/changed lines for
  existing API.
- edoc `%% @doc` on `parse_interval/1`, `interval_bounds/1`, `format_interval/1`.

## PR

`gh pr create` against `main`. Body: `Closes #3`; summarize the new API
(`parse_interval/1`, `interval_bounds/1`, `format_interval/1`), the four forms,
both separators, inheritance, and signed-duration support. Note `vsn` left at
1.3.4 (Duncan bumps to 1.4 at release). Close with a per-criterion disposition;
name anything not verified by running.
