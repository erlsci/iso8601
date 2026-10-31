# CC brief — drive iso8601 test coverage to 100%

**Goal:** get `iso8601` (the single module `src/iso8601.erl`) to **100% line
coverage**, honestly — real tests that exercise real behaviour, not coverage
theatre. Read `CLAUDE.md` first; it is the operative contract for this repo.

## Ground rules (from CLAUDE.md — non-negotiable)

- **Load the Erlang skill first.** Read `priv/ai/erlang/SKILL.md` and follow its
  loading instructions; load `11-anti-patterns.md` before writing anything, and
  `15-testing.md` for the eunit/common_test/PropEr conventions.
- **Do not change the public API or exported types to chase coverage.** The public
  surface (`format/1`, `parse/1`, `parse_exact/1`, `add_time/4`, `subtract_time/4`,
  `add_days/2`, `add_months/2`, `add_years/2`, `parse_duration/1`,
  `apply_duration/2`) and the exported `calendar`-style types are a stable contract.
  This is a widely-depended-on library; preserve backward compatibility.
- **Coverage is earned, not gamed.** "Write to the floor": a line counts as covered
  only when a test genuinely drives it. If a line looks impossible to reach, do not
  paper over it — either (a) prove with a real input that it *is* reachable and test
  it, or (b) if it is genuinely dead code, propose removing it (dead code hides bugs;
  flag it, don't quietly delete behaviour). If you hit a true ceiling, say so
  explicitly and name the exact uncovered lines — don't report a vague "done".

## Step 1 — establish the baseline

The sandbox has no Erlang toolchain locally; you'll run rebar3 yourself.

The repo's `coverage` alias only collects cover over the PropEr pass. To get a
report that reflects **all** tests, run eunit and proper both under cover:

```
rebar3 as test do eunit -c, proper -c, cover -v
```

Then open the annotated report and find every uncovered (red) line:

```
_build/test/cover/iso8601.COVER.html      # per-line annotation
_build/test/cover/index.html              # summary
```

Record the starting percentage and the full list of uncovered lines before you
write any new tests. That list is your worklist.

## Step 2 — close the gaps

Add tests to `test/iso8601_tests.erl`. From a static read, these look under-tested
(confirm against the actual report — don't trust this list blindly):

- **`format/1`** — no named generator exists. Cover the binary output for: the
  `is_float(S)` seconds clause, the integer-seconds clause, and the timestamp
  (`{_,_,_}`) input clause.
- **`add_days/2`, `add_years/2`, `apply_duration/2`** — no named generators.
- **Timestamp-tuple input clauses** of `add_time/4`, `add_days/2`, `add_months/2`,
  `add_years/2` (the `{_, _, _}=Timestamp` heads, as opposed to datetime tuples).
- **Parser error / fall-through clauses** — the `(_, _)` last clauses in `year`,
  `month_or_week`, `week_day`, `month_day`, `hour`, `minute`, `second`,
  `offset_hour`, `offset_minute`, etc. These are the "malformed input crashes
  cleanly" paths; drive them with bad input and assert the crash (eunit
  `?assertError`/`?assertException`), matching the library's let-it-crash stance.
- **Leap-year / ordinal edges** — `is_leap_year/1` (century vs 400-year rule),
  `unpack_ordinal_date`, `find_last_valid_date/1` (e.g. month-end rollover like
  adding a month to Jan 31).
- **Decimal/fractional paths** — `*_decimal` and the `,` vs `.` fractional
  separators in `hour`/`minute`/`second`.

Prefer table-style eunit generators (`_test_()` returning a list of assertions),
matching the existing file's style. Where a property reads more naturally than
enumerated cases (e.g. `parse` ∘ `format` round-trips), add a PropEr property
rather than hand-enumerating.

## Step 3 — verify the full gate

Before declaring done, the local gate must be clean (this is the `Before
submitting` checklist in CLAUDE.md):

```
rebar3 compile        # clean — fix all warnings even though they aren't errors
rebar3 xref           # clean
rebar3 dialyzer       # clean
rebar3 eunit -v       # green
rebar3 as test do eunit -c, proper -c, cover -v   # confirm 100%, properties pass
```

`rebar3 check` runs compile + xref + dialyzer + eunit + coverage in one shot; use
it as the final confirmation.

## Step 4 — report honestly

Report the before/after coverage numbers and a per-area disposition:

- For each previously-uncovered region: `covered` (name the test that drives it),
  or `removed-as-dead` (with rationale), or `still-uncovered` (with the exact line
  numbers and *why* it can't be driven).
- Distinguish "verified by running X" from "I believe X". Do not mark your own
  work verified — that's the reviewer's job.

**Optional, for Duncan to decide — do not do unilaterally:** once real coverage is
at 100%, propose raising the `coverage` alias gate in `rebar.config` from
`--min_coverage=0` to a hard floor (e.g. `--min_coverage=100`) so CI defends the
result. Flag it as a proposal; the gate change is a policy call, not part of this task.
