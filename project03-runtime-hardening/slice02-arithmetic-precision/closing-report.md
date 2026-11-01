# Slice02 CC closing report

Status: **CC proposed-done; CDC verification and Operator acceptance remain open.** This is a self-review and attestation, not independent acceptance.

## Assignment and pins

- Active assignment: refinement02, `cc-prompt-iteration02.md`, planning revision `0217b79addc5ea5084279788e36134c9f482e3c0`; current plan v1.2. Two refinements were issued, within the max5 allowance.
- Source branch/worktree: `codex/project03-arc02-arithmetic` at `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project03-arc02-arithmetic`; initial baseline `3a1cdfd17bd4dfaf36c9eaa34c5102944a3e0828`, tree `3224b1e0da9285ab67349aadc3b59ffa11bfe8ab`.
- Preserved initial unvalidated source checkpoint: `e64c20f9d718f012c451bcc3af96fce108368c5c`, tree `3f3d45a98abb5c00f3e31d4efd25271f5de91c48`. The early pre-intake edits, chronology, corrections, and superseded receipts remain described in `artifacts/runlog.md`.
- Final local source commit/tree: `d3c7012e1802a7b563f53fc9dcf3c8ad78ab3094` / `26e5fc47d163ecea39627abb25a4ba4f85bdbe19`. The commit contains exactly the three allowlisted paths, the required two adjacent trailers, and its worktree is clean.

## Result

Within CAL001/CAL002/DATE003/DATE005, month/day/year/time arithmetic now rejects below-year-zero targets before conversion or recursive clamping, while preserving valid year-zero targets and existing clamps. Integer calendar conversion keeps fractional anchors separate from large epoch seconds. Exact fractional hour/minute parsing retains its computed remainder without changing legacy `parse/1` rounding of fractional units or truncation of explicit fractional seconds. A private `combine_fraction/2` carries only when a canonical integer base clock plus a nonnegative fractional remainder reaches a represented second60; raw malformed expanded clocks retain their original values, and `parse/1` is not routed through the new normalizer. Float formatters round local seconds to integer microseconds before carrying through minute/hour and proleptic date boundaries.

Only the allowlisted `src/iso8601.erl`, `test/iso8601_tests.erl`, and `README.md` changed. Public exports, the11 exported type declarations, public specs/type aliases, and app version1.4.0 remain unchanged. The README now describes exact fractional-unit precision, legacy parsing distinctions, formatter rounding, arithmetic floor behavior, the year-zero `add_time/4` restriction, and the corrected `.50` example.

## Verification

- Eight sequential native gates passed on Erlang/OTP28.1.1, ERTS16.1.1, rebar3.27.0: compile; xref; Dialyzer with no warnings; EUnit296/296; PropEr4/4 properties ×100 cases; cover100%; formatter check; linter. Exact outputs are the `artifacts/refinement02-native-*.log` files.
- Final EUnit red/green comparison: unchanged baseline production compiled fresh from3a1cdfd with the current296-case test module yielded261 passed/35 failed; final candidate yielded296 passed/0 skipped. Two earlier pre-repair red runs are separately retained as257/22 and258/27. An early Dialyzer run exposed an overly broad private datetime spec; the unnecessary spec was removed and the final clean gate passed.
- Final immutable replay used `erlc -o /private/tmp/iso8601-s02-final-ebin src/iso8601.erl` from source SHA `bd0a93f6f6bc8e1a7d0833d4a156eeda37cd76088b1280cd66a3bf79fbec4302`; compiled beam SHA `a38eb2be69f9a6b2d8963754a4ecc4abaf2e0ff8f5873bf6f8e86e9fa5a4da88`. All147 observations ran:31 date,10 fractional-unit,42 original calendar,64 refined calendar. Four underflow workers promptly returned `badarg`. The first replay attempt loaded a stale `_build/default` beam; those logs remain preserved as invalid and were not overwritten.
- Raw-clock source-compatibility observations using the final EUnit beam preserve `25:00:00.0` and raw `00:00:60.0` for negative year and year0 inputs. These are recorded comparisons, not new malformed-input tests. Matched positive-year fractional-unit-plus-zone succeeds with expected `23:59:0.36`; the corresponding valid year-zero nonzero-offset input remains `badarg`.
- `git diff --check` is clean. Source diff inspection shows no changes to exports, exported types, public declarations, parser return aliases, or version.

## Nine-ID applicability walk

| Finding | CC disposition | Evidence and limit |
|---|---|---|
| ERL-CAL-001 | Proposed repaired | Month floor and bounded hang controls in final refined-calendar replay; CDC reproduction pending. |
| ERL-CAL-002 | Proposed repaired | Fractional anchors compose with integer shifts/durations without epoch-float arithmetic; final EUnit and refined-calendar replay; CDC reproduction pending. |
| ERL-CAL-003 | Retained prior accepted fix | Formatter-produced `+2007` inherited and explicit endpoints retain year2007 in final replay. |
| ERL-CAL-004 | Retained prior accepted fix | Zoned full-second abbreviated endpoint retains the start zone in final replay; minute-precision zoned-start candidate remains deferred. |
| ERL-DATE-001 | Open/deferred | Invalid week0, week53 for 2014, and weekday8 still normalize in final date replay. |
| ERL-DATE-002 | Open/deferred | Invalid expanded dates still return tuples in final date replay. |
| ERL-DATE-003 | Proposed repaired | Both float formatters carry rounded microseconds to the next date; format→parse control passes. |
| ERL-DATE-004 | Open/deferred | Nested iodata still raises `badarg` in all three public parser probes. |
| ERL-DATE-005 | Proposed repaired | All10 immutable unit probes match within1e-12; both legacy parse distinctions pass. |

## Remaining authority and handoff

S02-01 through S02-07 are CC proposed-done only. S02-08 remains open for parent-owned publication and actual OTP20–29 hosted jobs; S02-09 remains open for CDC independent reproduction, row disposition, and retrospective. Parent P03-14 and Operator P03-15 remain separate. No hosted CI, push, PR, merge, release, or later parser work was initiated. The source commit body was read back from the committed object; both trailers are adjacent and the committed tree is clean. The planning artifact/ledger commit pin will be appended after its separate named-path commit verification. CC then releases mutation ownership to CDC and stops.
