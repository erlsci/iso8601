# Slice01 CC close — interval inheritance repair

Date: 2026-10-31. CC disposition: **proposed complete for implementation and evidence; CDC verification, parent composition, and Operator checkpoint remain open.**

## Candidate and scope

Source candidate: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project03-arc01-intervals`, branch `codex/project03-arc01-intervals`, commit `ec665b8d7fd554969016f6e5014844d6108c98d0`. Baseline: `bcfd94a08fa7a427eb1469679897e4d92d808edf`. Red-only test commit: `f344400a81868c94a17728691c83f1943b1836e8`. Planning baseline at intake: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/planning`, branch `planning`, commit `0cc658082ef9bde17c723cfdd3f588b75fc432c4`; before this close, parent added and I preserved the disjoint denominator-correction commit `4fc7f8771ca0dc9247048a3912e65b9cbc366f96`. This close packet is the only mutation set from this CC on the current planning head. The source worktree is clean at the candidate commit.

Changed source paths are exactly `src/iso8601.erl`, `test/iso8601_tests.erl`, and `README.md`. The implementation retains raw signed positive-year tokens during classification and abbreviated date reconstruction; it inherits a raw start timezone only for an abbreviated end that omits one, after completing only missing local time components. Explicit end zones and complete endpoints remain independently parsed. Public exports/types, negative-year endpoint rejection, basic-start abbreviation rejection, the minute-precision zoned-start parser boundary, and unrelated arithmetic/date/parser findings remain unchanged.

## Red/green and validation

The test-only commit was made before production changes. Against unchanged `bcfd94a`, the corrected red EUnit run recorded **18 failures, 217 passes, 0 skipped**. The earlier preserved run had 19 failures because two assertions initially selected the interval tag instead of the endpoint; that test-authoring error was corrected before the corrected red run. Final candidate EUnit records **238 passed, 0 failed/skipped**. The first 18 red cases and exact baseline outputs are preserved in `artifacts/red-eunit-corrected.log`; candidate output is in `artifacts/native-eunit-final.log`.

Candidate-specific native checks passed on OTP 28 / ERTS 16.1.1 and rebar3 3.27.0: `rebar3 compile`, `rebar3 xref`, `rebar3 dialyzer`, `rebar3 eunit -c -v` (238 tests), `rebar3 proper -c` (4/4 properties, 100 generated tests each), `rebar3 as test cover -v --min_coverage=100` (100% module and total), `rebar3 as dev fmt --check`, and `rebar3 as dev lint`. Exact receipts and the formatter provider-recovery attempts are in `artifacts/native-*.log`. An accidental overlapping compile/xref invocation was preserved, then both commands were rerun sequentially; only the sequential receipts count as final evidence. EPP comparison confirms the exact baseline 15 exports and 11 exported types (`artifacts/epp-api-types.log`). `git diff --check` passed.

The rebar100% coverage gate is distinct from the Makefile's95% line-coverage script; neither threshold was changed. This local OTP28 run is not an OTP20–27 CI matrix or R15B03 compatibility proof. No release, publication, PR, push or merge was attempted.

## Audit replay applicability

All four immutable public probe runners were replayed against candidate beams. Date: **31/31**; fractional-unit comparisons: **10/10**; initial calendar: **42/42** (the runner contains 38 ordinary probes plus 4 bounded calls although the issued prompt says “38 initial calendar calls”); refined calendar: **64/64** (58 ordinary probes, 2 stack probes, 4 bounded calls). Candidate outputs are retained in `artifacts/replay-*.log`. The original audit runners were not edited; only temporary date-runner copies had their BeamDir changed. Each bounded hang probe confirmed the spawned worker was killed and its monitor `DOWN` observed.

| Finding | Changed-candidate disposition |
|---|---|
| ERL-CAL-001 | Still applicable: bounded year/month/interval underflow probes time out; workers are killed and `DOWN` is confirmed. Open for later increment. |
| ERL-CAL-002 | Still applicable: fractional-second anchors still raise `function_clause` in duration composition/anchored bounds; integer controls pass. Open for later increment. |
| ERL-CAL-003 | Corrected: short, padded and large signed years are retained in abbreviated and complete endpoints; formatter-produced `+2007` roundtrips; bare unsigned four digits remain a full year. CC proposed complete, CDC review open. |
| ERL-CAL-004 | Corrected within scope: omitted abbreviated-end zone inherits raw start zone; explicit zone/full endpoint controls, raw date boundary crossings, midnight, minute/hour/basic time, and fractions pass exact public assertions. Minute-precision zoned start remains a pre-inheritance parser rejection. CC proposed complete, CDC review open. |
| ERL-DATE-001 | Still applicable: invalid week zero/week 53/weekday values normalize to dates. Open for later increment. |
| ERL-DATE-002 | Still applicable: impossible nonpositive-year dates remain accepted in expanded parsing. Open for later increment. |
| ERL-DATE-003 | Still applicable: float rounding can emit second60 and parse across a date boundary. Open for later increment. |
| ERL-DATE-004 | Still applicable: nested iodata remains rejected by three parse APIs; flat list/binary controls pass. Open for later increment. |
| ERL-DATE-005 | Still applicable: six of ten fractional-hour/minute comparisons still miss the expected subsecond tolerance; whole-second and explicit-second controls pass. Open for later increment. |

The refinement only adds three post-red classifier edge tests—valid signed year-only full endpoint and empty/malformed signed endpoint rejection—after the coverage report showed those branches were otherwise unexercised. They are part of the final green suite, not claimed as red cases. Coverage failures/retries and exact provider versions are retained in the runlog.

## Ledger and handoff

Slice ledger rows S01-01 through S01-07 are marked `CC-proposed-done` with their evidence paths. S01-08 remains open for CDC's independent verification/retrospective and close. Project03 ledger rows P03-01/02 remain open pending independent verification; all other seven finding rows remain open. Parent-composition P03-10 and Operator P03-11 remain open, as do project acceptance/closure obligations. No plan or reviewer record was changed. The source diff remains within the accepted CAL003/CAL004 scope; no new finding or next increment is proposed.

See `artifacts/runlog.md` for corrected intake extents, exact command attempts/statuses, artifact inventory, source-cited readback and detailed replay dispositions. Stop here for CDC review and parent/Operator checkpoint.
