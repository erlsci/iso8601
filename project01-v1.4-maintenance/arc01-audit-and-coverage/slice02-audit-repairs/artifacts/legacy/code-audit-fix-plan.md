# CC brief — code-audit fix round (targets the 1.4 release)

Fix the confirmed findings from `workbench/2026.05.25-audit-results-erlang.md`,
TDD-style, on a branch, behind GitHub issues, in one PR. Read `CLAUDE.md` and
`priv/ai/erlang/SKILL.md` (anti-patterns first) before touching code.

## Hard constraints — read twice

- **iso8601 has 1.5M+ Hex downloads. NO breaking changes.** Do not change any
  exported function's signature, the `-export` list, the `-export_type` list, or
  any documented return shape. The public API surface must be byte-for-byte
  identical (verify with a diff at the end).
- **Behavior may change only where it corrects a clear bug** (F-1, F-2). Each such
  change must be called out explicitly in the PR description so the 1.4 release
  notes can mention it. A correct result replacing a wrong/invalid one is a fix,
  not a break — but it IS observable, so document it.
- **Do not bump the version.** `vsn` stays `1.3.4`; Duncan bumps `1.3.4 → 1.4`
  separately after merge.
- **No CHANGELOG** (house rule). Release notes live in GitHub releases.
- **OTP 20+ floor holds.** No post-OTP-20 BIFs/syntax. (The audit confirmed the
  module is clean here — don't regress it.)
- Process: you implement and self-assess; a separate reviewer verifies by
  re-running commands. Do not mark your own work verified. Write to the floor.

## Step 1 — Issues: REUSE before you create

Existing open issues were fetched on 2026-05-25; **re-confirm with
`gh issue list --repo erlsci/iso8601 --state open` before doing anything** (the
list may have moved). Map findings to issues:

| Finding | Existing issue? | Action |
|---|---|---|
| F-1 apply_duration ignores sign | **#48** (open since 2018, exact match) | **Reuse #48.** Do NOT open a new one. |
| F-2 add_years returns invalid dates | none found | Create new (draft below). |
| F-3 debug `io:format` in parse path | none found | Create new (draft below). |
| F-7/F-9 cleanups | none found | Create one "code-audit cleanups" issue, OR fold into the PR with no ticket — your call, but prefer one small issue so the PR closes something. |
| (bonus) #4 "PropEr tests" (milestone 1.4) | **#4** | Already satisfied by `test/prop_iso8601.erl`. Reference & **Closes #4** in this PR. |

If `gh auth status` fails, don't block: write the new-issue drafts to
`workbench/new-issues.md` for Duncan to file, and leave `Closes #NN` placeholders
in the PR body. Capture every issue number you use.

### New-issue draft — F-2

> **Title:** `add_years/2` and `apply_duration/2` can produce invalid dates (Feb 29)
>
> **Body:** Adding/subtracting years to a Feb-29 date doesn't clamp to a valid
> day. `iso8601:add_years({{2016,2,29},{0,0,0}}, 1)` returns `{{2017,2,29},...}` —
> `calendar:valid_date/1` reports that `false`. `apply_years_offset/2` skips the
> `find_last_valid_date/1` normalization that `apply_months_offset/2` already
> applies, so the months path clamps (Jan 31 + 1mo → Feb 28) but the years path
> doesn't. Fix: route the years result through `find_last_valid_date/1` too.

### New-issue draft — F-3

> **Title:** Debug `io:format` output on every ordinal-date parse
>
> **Body:** `src/iso8601.erl` has two leftover `io:format/1,2` debug calls on the
> ordinal-date parse path (e.g. `iso8601:parse("2015-201")`), printing
> `"Ordinal date, no time!"` / `"Ordinal date, time is ..."` to stdout. A library
> must not write to a caller's stdout. Fix: delete both lines.

## Step 2 — Branch & TDD loop

Branch `task/code-audit-fixes` off `main` (CI fires on `task/**`). For **each**
finding, in order: write the failing test(s) → run and **see them fail** → apply
the fix → run and **see them pass**. Don't write the fix before the red test.

Tests go in `test/iso8601_tests.erl` unless noted. Run with
`rebar3 eunit -v` (single module: `rebar3 eunit -v -m iso8601_tests`).

---

### F-1 — `apply_duration/2` ignores the sign (Issue #48, High)

`apply_duration/2` matches the parsed sign as `_S` and discards it, so a negative
ISO duration is applied as positive.

**Red tests** (compute/confirm expected values by running; the simple case is
certain):

```erlang
apply_duration_sign_test_() ->
    [%% negative year subtracts
     ?_assertEqual({{2016,5,24},{1,2,3}},
                   iso8601:apply_duration({{2017,5,24},{1,2,3}}, "-P1Y")),
     %% regression: positive/unsigned still adds (these already pass today)
     ?_assertEqual({{2018,5,24},{1,2,3}},
                   iso8601:apply_duration({{2017,5,24},{1,2,3}}, "P1Y"))].
```

Add at least one compound negative case (e.g. `"-P1M1DT1H1M1S"`) — derive its
expected value by running, don't guess.

**Fix** (`apply_duration/2`): read the sign, negate every component when `"-"`:

```erlang
apply_duration(Datetime, Duration) ->
    [{sign, Sign}, {years, Y}, {months, M}, {days, D},
     {hours, H}, {minutes, MM}, {seconds, SS}] = parse_duration(Duration),
    F = case Sign of "-" -> fun(X) -> -X end; _ -> fun(X) -> X end end,
    D1 = apply_years_offset(Datetime, F(Y)),
    D2 = apply_months_offset(D1, F(M)),
    D3 = apply_days_offset(D2, F(D)),
    apply_offset(D3, F(H), F(MM), F(SS)).
```

Note: I checked the negative path — `apply_months_offset/2`'s `div`/`rem` math
handles negative offsets correctly across year boundaries (e.g. Jan − 1mo → prev
Dec), and `apply_days_offset`/`apply_offset` are gregorian-seconds based, so
negatives are fine. Confirm with the compound test.

---

### F-2 — `add_years`/`apply_years_offset` can return invalid dates (new issue, High)

**Red tests:**

```erlang
add_years_leap_day_test_() ->
    [?_assertEqual({{2017,2,28},{0,0,0}}, iso8601:add_years({{2016,2,29},{0,0,0}}, 1)),
     ?_assertEqual({{2015,2,28},{0,0,0}}, iso8601:add_years({{2016,2,29},{0,0,0}}, -1)),
     ?_assert(calendar:valid_date(element(1, iso8601:add_years({{2016,2,29},{0,0,0}}, 3)))),
     %% regression: ordinary dates unchanged (already passes)
     ?_assertEqual({{2018,5,24},{1,2,3}}, iso8601:add_years({{2017,5,24},{1,2,3}}, 1)),
     %% and via apply_duration (years path)
     ?_assertEqual({{2017,2,28},{0,0,0}}, iso8601:apply_duration({{2016,2,29},{0,0,0}}, "P1Y"))].
```

**Fix** (`apply_years_offset/2`) — mirror what the months path already does:

```erlang
apply_years_offset(Datetime, AY) ->
    {{Y, M, D}, {H, MM, S}} = Datetime,
    find_last_valid_date({{Y + AY, M, D}, {H, MM, S}}).
```

This clamps Feb 29 → Feb 28 in non-leap years, consistent with `add_months`. No
existing test exercises a leap-day year offset, so nothing should regress —
confirm.

---

### F-3 — debug `io:format` in parse path (new issue, High)

The two calls are around the ordinal-date `month_or_week` clauses (currently
~lines 185, 189), both standalone statements ending in `,` — safe to delete.

**Red test** — capture stdout during an ordinal parse and assert it's empty. Add
this capture helper (adjust io-protocol message forms if needed — run it and
confirm it goes red, then green):

```erlang
ordinal_parse_no_stdout_test() ->
    {Result, Output} = capture_output(fun() -> iso8601:parse("2015-201") end),
    ?assertMatch({{2015,7,20},{0,0,0}}, Result),  %% confirm the actual date by running
    ?assertEqual("", Output).

capture_output(Fun) ->
    Old = group_leader(),
    Self = self(),
    Cap = spawn_link(fun() -> cap_loop(Self, []) end),
    group_leader(Cap, self()),
    R = Fun(),
    group_leader(Old, self()),
    Cap ! {stop, self()},
    receive {captured, O} -> {R, O} end.

cap_loop(Owner, Acc) ->
    receive
        {io_request, From, ReplyAs, {put_chars, _E, Chars}} ->
            From ! {io_reply, ReplyAs, ok}, cap_loop(Owner, [Chars | Acc]);
        {io_request, From, ReplyAs, {put_chars, _E, Mod, Fun, Args}} ->
            From ! {io_reply, ReplyAs, ok}, cap_loop(Owner, [apply(Mod, Fun, Args) | Acc]);
        {io_request, From, ReplyAs, _Other} ->
            From ! {io_reply, ReplyAs, ok}, cap_loop(Owner, Acc);
        {stop, Owner} ->
            Owner ! {captured, lists:flatten(lists:reverse(Acc))}
    end.
```

**Fix:** delete both `io:format(...)` lines. Leave the `io_lib:format` calls in
`format/1` (~lines 88, 92) — those are legitimate. Confirm the test goes green.

---

### F-7 — unused macros (Low)

`?MIDNIGHT`, `?NOON`, `?TEATIME` (lines ~16–18) are defined and never referenced.
Delete them. No test; `rebar3 compile` must stay clean and all tests still green.

### F-9 — `gi/1` cosmetics (Low) — and a DANGER about F-4

Fix only the malformed doc comment: `%doc ...` → `%% @doc ...`. Optionally rename
the private `gi/1` to something legible (e.g. `capture_to_integer/1`) and update
all 6 call sites — it's private, so this is non-breaking.

**Do NOT change `gi/1`'s conversion logic.** The audit's F-4 proposed
`gi(DS) -> list_to_integer(DS)` — **reject it.** `gi/1` is applied to duration
seconds, which can be fractional (`"1.1"`); `list_to_integer("1.1")` crashes,
which would break `parse_duration("..1.1S")` — behavior the README documents
(`{seconds, 1}`). The current `string:to_integer/1` + `0`-fallback is correct, and
its failure branch is unreachable through the public API (the regex guarantees the
input), so there's no real bug to fix and a real regression to avoid. Leave it.

Also correct two audit mistakes so they don't get "fixed":
- **F-9 wording:** `gi/1` is **not** exported — it's private. Only the comment/name
  nit applies; ignore the "exported" framing.
- **F-5 is a false finding:** `calendar:now_to_datetime/1` is **not** deprecated.
  xref runs with `deprecated_function_calls`/`deprecated_functions` enabled and
  passes, which proves it. **Do not touch `calendar:now_to_datetime/1`.**

### F-10 — `?assertMatch` → `?assertEqual` (Low) — SEPARATE PR

Mechanical change across ~100 assertions; it would bloat this PR's diff and bury
the real fixes. Leave it out of this PR. If Duncan wants it, do it as its own PR.

## Step 3 — Verify before opening the PR

- `rebar3 compile` clean (no warnings), `rebar3 xref` clean, `rebar3 dialyzer` clean.
- `rebar3 eunit -v` green; properties run (`rebar3 proper -c` reports a non-zero
  property count — not `0/0`); coverage still **100%** (the new tests cover the
  fixed lines).
- **Backward-compat gate:** `git diff main -- src/iso8601.erl | grep -E '^\-.*(-export|-spec|-type)'`
  must show no removed/changed export/spec/type lines. The public surface is
  unchanged; only function bodies and tests differ.

## Step 4 — PR

`gh pr create` against `main`. In the body:
- `Closes #48` (F-1), `Closes #<F2>`, `Closes #<F3>`, `Closes #<cleanups>`, and
  `Closes #4` (PropEr tests now exist).
- A **Behavior changes** section, stated plainly for the release notes:
  - Negative ISO durations are now subtracted (previously applied as positive).
  - `add_years`/`apply_duration` on Feb 29 now clamps to Feb 28 in non-leap years
    (previously returned an invalid date / rolled into March).
  - No more stdout output when parsing ordinal dates.
- Note the version is intentionally left at `1.3.4` (Duncan bumps to 1.4 separately).

Close with a per-finding disposition (done + evidence / deferred + reason), not a
prose summary. Name anything you couldn't verify by running.
