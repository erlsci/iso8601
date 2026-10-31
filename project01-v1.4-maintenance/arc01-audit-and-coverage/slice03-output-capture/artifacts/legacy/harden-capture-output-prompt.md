# CC brief — harden `capture_output/1` test helper

In `test/iso8601_tests.erl`, the `capture_output/1` helper (added for the F-3
stdout test) restores the group leader only on the success path:

```erlang
capture_output(Fun) ->
    Old = group_leader(),
    Self = self(),
    Cap = spawn_link(fun() -> cap_loop(Self, []) end),
    group_leader(Cap, self()),
    R = Fun(),
    group_leader(Old, self()),          %% skipped if Fun() raises
    Cap ! {stop, Self},
    receive {captured, O} -> {R, O} end.
```

If `Fun()` ever raises, the group leader is never restored, so a later test's
stdout silently routes into the (now orphaned) capture process — a confusing,
order-dependent failure. Harden it so the leader is always restored.

## Fix

Wrap the redirect-and-run body in `try ... after ... end`:

```erlang
capture_output(Fun) ->
    Old = group_leader(),
    Self = self(),
    Cap = spawn_link(fun() -> cap_loop(Self, []) end),
    group_leader(Cap, self()),
    try
        R = Fun(),
        Cap ! {stop, Self},
        receive {captured, O} -> {R, O} end
    after
        group_leader(Old, self())
    end.
```

## Constraints / rationale

- **Keep it OTP 20-safe.** Use a bare `try ... after ... end` — do NOT add a
  `catch Class:Reason:Stacktrace` clause (3-arg stacktrace is OTP 21+) or
  `erlang:get_stacktrace/0` (deprecated OTP 21, removed OTP 24). The `after`
  block runs on both normal return and exception, and any exception re-propagates
  automatically with its original class/reason/stacktrace — no manual re-raise.
- **No manual `Cap` teardown needed on the error path.** `Cap` is `spawn_link`ed,
  so when an exception propagates and the eunit test process exits, the linked
  capture process is torn down with it. (On the success path it still exits
  normally after `{stop, _}`.) Leave `cap_loop/2` unchanged.

## Verify

- `rebar3 eunit -v` still green (the existing `ordinal_parse_no_stdout_test`
  exercises the success path).
- Add one small regression test proving the leader is restored after a raise,
  e.g.:

  ```erlang
  capture_output_restores_leader_on_error_test() ->
      Old = group_leader(),
      ?assertError(boom, capture_output(fun() -> error(boom) end)),
      ?assertEqual(Old, group_leader()).
  ```

- Confirm coverage stays at 100% and `rebar3 check` is clean. Test-only change —
  no `src/` edits, no public-surface impact.
```
