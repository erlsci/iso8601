# CC brief — fix two issues from the PR #73 review (expanded/negative years)

Follow-up on `task/expanded-year-negative` (commit `362f8e4`). The negative-year
feature is correct for the common cases, but the `apply_offset/4` short-circuit
that enables it is too broad and introduced two problems. Fix both, TDD, on the
same branch; amend PR #73. Read `CLAUDE.md` first.

## Background

To let year-≤0 dates avoid `calendar:datetime_to_gregorian_seconds` (which can't
represent them), `apply_offset/4` was changed to return the datetime unchanged
whenever the offset rounds to 0. But `apply_offset/4` is shared by `parse/1`,
`parse_exact/1`, `add_time/3`, intervals, and `apply_duration/2`, so that blanket
short-circuit has two unwanted effects:

- **F1 (regression for existing API):** the zero-offset path used to do a
  gregorian round-trip that *incidentally normalized* out-of-range dates. With the
  blanket short-circuit, positive-year parses no longer normalize — e.g.
  `parse("2017-02-30")` returned `{2017,3,2}` in 1.3.4 but now returns the invalid
  `{2017,2,30}`. Valid dates are unaffected (which is why tests still pass), but
  this is a silent behavior change to `parse/1`/`parse_exact/1` for malformed
  input. We want **1.3.4 behavior preserved** (no breaking change).
- **F2 (silent-wrong in the new feature):** a year ≤ 0 with a *non-zero* offset
  (timezone like `+02:00`, or a week-date) still routes through the calendar and
  miscomputes silently — `parse_expanded("-0044-03-15T12:00:00+02:00")` returns a
  wrong, positive-year date instead of erroring.

## The fix — scope the bypass to years calendar can't represent

Replace the blanket "offset rounds to 0 → return as-is" with a year-aware version.
For `Y >= 1`, always use the calendar path exactly as 1.3.4 did (this restores the
zero-offset round-trip and its normalization). Only year ≤ 0 bypasses, and only a
zero offset is safe there; a non-zero offset on year ≤ 0 is `badarg`.

```erlang
apply_offset({{Y, _, _}, _} = Datetime, H, M, S) ->
    OffsetS = S + (60 * (M + (60 * H))),
    Rounded = round(OffsetS),
    case {Y < 1, Rounded} of
        {false, _} ->
            %% positive year: calendar handles it; preserve 1.3.4 behavior
            %% (including the zero-offset round-trip that normalizes out-of-range dates)
            Gs = Rounded + calendar:datetime_to_gregorian_seconds(Datetime),
            calendar:gregorian_seconds_to_datetime(Gs);
        {true, 0} ->
            %% year <= 0: calendar can't represent it, but a zero offset is a no-op
            Datetime;
        {true, _} ->
            %% year <= 0 with a real offset: calendar can't compute this correctly
            error(badarg)
    end.
```

This is the whole behavioral fix; both findings fall out of it.

## Tests (TDD: red → green)

### F1 — positive-year normalization is restored (1.3.4 behavior)

A relational assertion avoids hard-coding the day arithmetic (confirm by running):

```erlang
parse_normalizes_out_of_range_positive_test_() ->
    %% out-of-range day normalizes through the calendar, as in 1.3.4
    ?_assertEqual(iso8601:parse("2017-03-02"), iso8601:parse("2017-02-30")).
```

Under the current PR this fails (LHS `{2017,3,2}`, RHS `{2017,2,30}`); after the
fix it passes. Also confirm the existing 205 tests still pass (they exercise the
valid-date paths).

### F2 — year ≤ 0 with a non-zero offset is `badarg`

```erlang
parse_expanded_negative_offset_test_() ->
    [%% negative year + timezone offset can't be computed → badarg, not silent-wrong
     ?_assertError(badarg, iso8601:parse_expanded("-0044-03-15T12:00:00+02:00")),
     %% negative year + week date (non-zero week offset) → badarg
     ?_assertError(badarg, iso8601:parse_expanded("-0001-W01-1")),
     %% sanity: negative year in UTC / plain date still works (zero offset)
     ?_assertMatch({{-44,3,15},{12,0,0.0}}, iso8601:parse_expanded("-0044-03-15T12:00:00Z")),
     ?_assertMatch({{-1,1,1},{0,0,0.0}},   iso8601:parse_expanded("-0001-01-01"))].
```

(Confirm the exact `Z`-case tuples by running; the point is offset==0 negatives
work, non-zero-offset negatives raise.)

## Docs

- Add a sentence to `parse_expanded/1`'s edoc: negative/astronomical years are
  supported in UTC (`Z`) or offset-free form; combining a negative year with a
  non-zero UTC offset or a week-date raises `badarg`, since the proleptic
  conversion isn't defined here.
- Add the same caveat as one line to the expanded-year section of `README.md` on
  this branch.

## Verify

- `rebar3 compile` clean (no warnings), `xref` clean, `dialyzer` clean.
- `rebar3 eunit -v` green (incl. the restored normalization and the new `badarg`
  cases); `rebar3 proper -c` non-zero property count; coverage still **100%**
  (all three `apply_offset/4` branches are now reached — positive, year≤0 zero
  offset, year≤0 non-zero offset).
- Backward-compat: `parse/1`/`parse_exact/1` behavior now matches 1.3.4 again for
  all positive years (the F1 regression is gone); no public surface change beyond
  what PR #73 already added.

Amend PR #73 (still `Closes #72`). Closing note: per-finding disposition (F1
restored-to-1.3.4 + test; F2 guarded with badarg + test + docs), and name anything
not verified by running.
