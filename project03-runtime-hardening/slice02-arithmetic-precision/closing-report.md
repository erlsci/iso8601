# Slice02 CC closing report

Status: **CC proposed-done; CDC verification and Operator acceptance remain open.** This is a self-review and attestation, not independent acceptance.

## Assignment and pins

- Current assignment completed: refinement04, `cc-prompt-iteration04.md`, planning revision `ceff1c56e21e5c5489a3babcc7536bc5860daeac`; current plan v1.4. Four refinements have been issued, within the max5 allowance.
- Source branch/worktree: `codex/project03-arc02-arithmetic` at `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project03-arc02-arithmetic`; initial baseline `3a1cdfd17bd4dfaf36c9eaa34c5102944a3e0828`, tree `3224b1e0da9285ab67349aadc3b59ffa11bfe8ab`.
- Preserved initial unvalidated source checkpoint: `e64c20f9d718f012c451bcc3af96fce108368c5c`, tree `3f3d45a98abb5c00f3e31d4efd25271f5de91c48`. The early pre-intake edits, chronology, corrections, and superseded receipts remain described in `artifacts/runlog.md`.
- Final CC source commit/tree: `7926049a5a0093f9ffe5cc2dc5b77c5e560629f4` / `67266fc36ae8ab7731af95d1bc9d6f786d382751`. Refinement04 changes only the two private `make_date` parameter annotations in `src/iso8601.erl`; committed message bytes have adjacent required trailers and the worktree is clean.

## Result

Within CAL001/CAL002/DATE003/DATE005, month/day/year/time arithmetic now rejects below-year-zero targets before conversion or recursive clamping, while preserving valid year-zero targets and existing clamps. Integer calendar conversion keeps fractional anchors separate from large epoch seconds. Exact fractional hour/minute parsing retains its computed remainder without changing legacy `parse/1` rounding of fractional units or truncation of explicit fractional seconds. A private `combine_fraction/2` carries only when a canonical integer base clock plus a nonnegative fractional remainder reaches a represented second60; raw malformed expanded clocks retain their original values, and `parse/1` is not routed through the new normalizer. Float formatters round local seconds to integer microseconds before carrying through minute/hour and proleptic date boundaries.

Only the allowlisted `src/iso8601.erl`, `test/iso8601_tests.erl`, and `README.md` changed. Public exports, the11 exported type declarations, public specs/type aliases, and app version1.4.0 remain unchanged. The README now describes exact fractional-unit precision, legacy parsing distinctions, formatter rounding, arithmetic floor behavior, the year-zero `add_time/4` restriction, and the corrected `.50` example.

## Verification

- At the prior d3 candidate, eight sequential native gates passed on Erlang/OTP28.1.1, ERTS16.1.1, rebar3.27.0: compile; xref; Dialyzer with no warnings; EUnit296/296; PropEr4/4 properties ×100 cases; cover100%; formatter check; linter. Refinement03 freshly reran compile, EUnit300/300, PropEr4/4×100, cover100%, formatter check, and lint; all exited0. Refinement04 freshly reran compile, Dialyzer, formatter check, and lint; all exited0. Logs are in the corresponding `refinement02-native-*`, `refinement03-native-*`, and `refinement04-native-*` artifacts. The r03/r04 executable-equality logs establish unchanged compiled function lists, supporting retained xref, test, PropEr, coverage, and replay applicability; CDC's final all8-gate run remains open.
- Current baseline red/green: unchanged production from3a1cdfd compiled fresh with the new 300-case module yielded261 passed/39 failed, exit1; current candidate yielded300 passed/0 skipped. The prior 296-case comparison was261/35. The exact four additions account for the difference in baseline failures. The timeout-wrapper miss is retained separately; direct bounded EUnit completed. Earlier pre-repair reds remain separately retained as257/22 and258/27.
- The 147-observation replay used `erlc -o /private/tmp/iso8601-s02-final-ebin src/iso8601.erl` from source SHA `bd0a93f6f6bc8e1a7d0833d4a156eeda37cd76088b1280cd66a3bf79fbec4302`; compiled beam SHA `a38eb2be69f9a6b2d8963754a4ecc4abaf2e0ff8f5873bf6f8e86e9fa5a4da88`. All147 observations ran:31 date,10 fractional-unit,42 original calendar,64 refined calendar. Four underflow workers promptly returned `badarg`. Refinement03 and refinement04 compiled-function equality support retaining these runtime receipts at d3; no fresh r04 replay is claimed. The first stale `_build/default` replay attempt remains preserved and invalid.
- Refinement03 attempted to recover the original local Dialyzer warning for private `datetime(parser_accumulator())`. The raw warning and exact local exit were not found in retained Slice02 scratch; only the clean rerun and a contemporaneous attestation survive. No later or unrelated P02 warning is substituted or reconstructed as original evidence.
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

S02-01 through S02-07 are CC proposed-done only. S02-08 remains open for parent-owned publication and actual OTP20–29 hosted jobs; S02-09 remains open for CDC independent all8-gate/147-observation reproduction, row disposition, and retrospective. Parent P03-14 and Operator P03-15 remain separate. No hosted CI, push, PR, merge, release, or later parser work was initiated. The refinement04 source message/tree and single-file diff were read back; both trailers are adjacent and the source worktree is clean. Planning evidence/ledger/report/runlog commit `18ee5cc77788e9929fcf49909de38458b6b8855c`, tree `2e45677fce4442c576b03795493983bcbc197f79`, was read back with adjacent required trailers. A final closeout-pin commit records this exact planning state; mutation ownership is released to CDC/parent.
