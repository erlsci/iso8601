# Slice01 CC closing report — date contracts

Date: 2026-10-31. This closes the initial evidence-only CC assignment in [`cc-prompt.md`](cc-prompt.md), assigned from planning commit `e2b11d8a7ace3d3902e275ccb3690608141034ec`.

## Revisions and changed-file lists

- Source checkout: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit`, branch `codex/project02-arc01-audit`, source revision `bcfd94a08fa7a427eb1469679897e4d92d808edf`. No source, test, build configuration, or documentation bytes were changed. Final `git diff --exit-code bcfd94a --` and `git status --short --branch` confirm no tracked source diff.
- Planning checkout: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/planning`, branch `planning`, base `e2b11d8a7ace3d3902e275ccb3690608141034ec`. This close packet will be recorded in the next local planning commit; its hash is pending until the commit completes.
- Exact intended planning commit files: `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/2026.10.31-audit-results-erlang-date.md`, `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/date-probes.escript`, `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/date-probes.out`, `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/runlog.md`, `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/closing-report.md`, and `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/ledger.md`. No issued prompt, slice/arc/project plan, historical archive, or reviewer record is included.

## Ledger walk (six opening rows)

The opening ledger has six rows; this report accounts for all six. Here `done — proposed by CC, attested` is explicitly not CDC acceptance. CDC's reproduction is pending. S01-06 remains open because its criterion includes reviewer-owned records not yet produced.

| ID | CC disposition | Evidence and strength | Notes |
|---|---|---|---|
| S01-01 | done — proposed, attested | [`artifacts/runlog.md`](artifacts/runlog.md), assignment/scope and full-reading inventory; source-cited contract readback | Required manifest embedded in the exact prompt; intake scope recorded before evidence generation. |
| S01-02 | done — proposed, attested | [`artifacts/2026.10.31-audit-results-erlang-date.md`](artifacts/2026.10.31-audit-results-erlang-date.md), API/helper map and six concrete clean checks | Four supported findings; no finding quota. CDC clarified “negative findings” means clean checks per code-auditing terminology. |
| S01-03 | done — proposed, attested | Audit report findings ERL-DATE-001–004; [`artifacts/date-probes.escript`](artifacts/date-probes.escript) and [`artifacts/date-probes.out`](artifacts/date-probes.out) | Each finding includes public calls, actual result, expected behavior grounded in source/docs or external primary references, and positive/negative controls. |
| S01-04 | done — proposed, attested | [`artifacts/runlog.md`](artifacts/runlog.md), complete native command/exit/population record | Compile/xref/Dialyzer/EUnit/PropEr/100% coverage passed locally on OTP28. Formatter/linter blocked by plugin package fetch/update errors. No OTP matrix or aggregate-alias pass claimed. |
| S01-05 | done — proposed, attested | Source baseline/hash and final no-diff/status evidence in runlog; this explicit file list | Diagnosis-only boundary held. Generated `_build` is fresh and ignored; no build output is part of planning commit. |
| S01-06 | open — CC portion attested; reviewer gate pending | This closing report's row walk and Bubble-up section are CC-attested | CDC must independently verify rows and record the assigned verification and retrospective. Re-enter after those reviewer-owned outputs exist; do not mark the slice accepted or advanced yet. |

## Validation results and limits

The report/run log preserve the exact commands and terminal observations. Fresh OTP 28 / ERTS 16.1.1, rebar3 3.27.0 results: `rebar3 compile`, `rebar3 xref`, `rebar3 dialyzer`, `rebar3 eunit -c -v` (210 tests), `rebar3 proper -c` (4/4 properties, 100 each), and `rebar3 as test cover -v --min_coverage=100` (100%) passed. `rebar3 as dev fmt --check` and `rebar3 as dev lint` stopped with exit 1 after plugin updates failed and commands were unavailable. One initial attempt to use shell `timeout` exited 127 before invoking Rebar; the immediately following Python-bounded compile passed. Rebar dependency archive fallback to existing cached package tarballs is recorded. No May-era beams/caches were used.

No `rebar3 check` or `make check` aggregate command is claimed. The Makefile executable-line gate is 95%, whereas configured CI/rebar coverage requires 100%; the thresholds remain untouched. Installed OTP28 is the only runtime tested, so OTP20–27 and historical R15B03 are unverified. No source changes or remediation were made.

## Artifact inventory

Durable supporting artifacts produced for this slice (all under `artifacts/`):

- `2026.10.31-audit-results-erlang-date.md` — date API findings, map, clean checks, validation and limits.
- `date-probes.escript` — bounded public API probe runner loading the fresh isolated-source beam path.
- `date-probes.out` — complete public probe transcript.
- `runlog.md` — intake, source-cited readback, evidence/rerun details, gate output, and clean-check record.

First-class slice files remain at the slice root: this `closing-report.md`, existing `ledger.md`, `slice-plan.md`, and issued `cc-prompt.md`. No reviewer artifacts were created.

## Bubble-up to the arc

1. **Assigned capability delivered:** Arc01 assigns Slice01 to audit the date/time public contract. This pass maps and probes all five assigned APIs and their shared date helpers. It found four reproducible issues: acceptance/normalization of invalid ISO week values; expanded-year calendar-date validation bypass for years `<= 0`; float formatting that rounds into second 60 without carrying; and nested iodata rejected despite `iodata()` specs. The usable outcome is a source-grounded, reproducible decision set for future disposition, not a source implementation. The source revision remains `bcfd94a08fa7a427eb1469679897e4d92d808edf`; only planning artifacts are proposed for local commit, so no source integration occurred.
2. **What the arc plan did not anticipate:** Date parsing's week conversion is arithmetic normalization without validating the supplied week/day tuple; expanded years use a zero-offset bypass that also skips date validation; and exact-six-place float rendering can cross a calendar boundary after rounding. These are localized audit discoveries; they do not change Arc01's stated slice breakdown or audit ordering. CDC should decide whether follow-up remediation belongs after Arc01's remaining runtime surfaces or in a later project slice. No arc-plan change is made by CC.
3. **Silent-drop diff:** All five assigned public APIs, required shared parsing/formatting helpers, representative input/output/error shapes, listed year/ordinal/week/fractional/offset cases, and five-plus clean checks were addressed. Read-only full-manifest files were loaded, but only the date API surface was analyzed, as assigned. No source fix, runtime scale beyond the synchronous library boundary, prior audit report, other public API family, or OTP20–27 matrix is represented as completed. Those limits are stated in the report/runlog; there is no undisclosed scope omission.

**CC verdict:** proposed-done/attested for S01-01 through S01-05. S01-06 remains open pending CDC's independent reproduction and retrospective. The slice is returning for review and is not accepted or advanced by this report.

## Refinement01 close update — 2026-10-31

This dated update executes `cc-prompt-iteration01.md`, issued from planning revision `3bebf0ac0d562c7fe4a06e18cf2e3142047cfdf2` after CDC returned initial candidate `3dc34cf1c815c2df579505e3570590303cfe3e8c` for original-scope corrections. Source remains `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit`, branch `codex/project02-arc01-audit`, baseline `bcfd94a08fa7a427eb1469679897e4d92d808edf`. The source worktree still has no tracked diff at that baseline. This iteration changes report evidence and close/status documentation only; the original runner/transcript and initial wording remain recoverable at the initial candidate.

### Six-row disposition after CC refinement

| ID | Current CC disposition | Evidence and strength | Remaining reviewer work |
|---|---|---|---|
| S01-01 | done — CC-proposed, attested | `artifacts/runlog.md`: embedded-manifest clarification, initial full-read inventory, current assignment and source/planning revision boundary | CDC may verify records during final review. |
| S01-02 | done — CC-proposed, attested | Updated API/helper map, five findings, scale coverage, and six concrete clean checks in `artifacts/2026.10.31-audit-results-erlang-date.md` | CDC checks current report scope and finding support. |
| S01-03 | done — CC-proposed, attested | ERL-DATE-001–005, explicit scales, corrected private-helper wording, initial transcript, and separate iteration01 public probe runner/output | CDC independently reruns the added probes and reviews expected/actual controls. |
| S01-04 | done — CC-proposed, attested | Initial gate receipts and CDC's independent OTP28 rerun remain preserved in `artifacts/runlog.md` and `cdc-verification.md`; no native rerun was needed for this focused refinement | Existing formatter/linter plugin block and unsupported OTP matrix remain limits. |
| S01-05 | done — CC-proposed, attested | Current source no-diff check; exact intended commit scope below; old attempts and initial four-finding packet remain pinned | CDC verifies committed paths and the source boundary. |
| S01-06 | open — CC close/bubble-up portion attested; reviewer gate pending | This dated row walk and scope-diff update; current disposition remains proposed, not accepted | CDC must perform final independent verification and retrospective, then record acceptance or a numbered correction. |

### Scope diff and findings

R1 closes the previously omitted fractional-hour/minute subsecond path in the original date-helper scope: `hour_decimal/2` and `minute_decimal/2` reach `add_decimal/2` and `apply_offset/4`, where whole-second rounding precedes the exact APIs' separate seconds-fraction addition. Ten bounded public calls produced three central arithmetic mismatches, the same mismatch in comma/basic-binary/expanded-year siblings, and three passing controls. This supports new ERL-DATE-005 (Medium, logical-unit scale; medium-high confidence with the documented fractional-unit precision caveat), without asserting a defect in `parse/1`'s integer-second contract. R2 corrects the original claim that private `make_date/4` had an exported spec; actual module exports are cited. R3 gives every finding an explicit scale and accounts for absent service/workspace scales without inventing findings. No arc-plan change is proposed; this is completion of the existing date-helper inspection, not new source behavior or remediation.

Current finding set: ERL-DATE-001–003 and ERL-DATE-005 are Medium runtime correctness issues; ERL-DATE-004 is Low, concerning nested input shapes broader than the prose examples. Original findings 001–004 keep their IDs. Initial report wording and evidence are preserved at candidate `3dc34cf1c815c2df579505e3570590303cfe3e8c`; the current report has a dated refinement history. The original `date-probes.escript` and `.out` are unchanged. New evidence is `artifacts/fractional-unit-probes-iteration01.escript` and `.out`; complete output, arithmetic derivation, tolerance, one corrected runner syntax error, and the current source-cited readback are appended to `artifacts/runlog.md`.

### Validation, inventory, and intended commit

The focused iteration probe command exited 0 against the fresh source-checkout beam built from unchanged `bcfd94a`; the separate output file records all ten calls and uses absolute float tolerance `1e-12`. Native gates were not repeated because this iteration does not alter source, tests, or build configuration and the refinement prompt explicitly says unchanged native gates need not be rerun; their original CC receipts and CDC's independent reproduction remain intact. The missing shell `timeout` attempt, network/cache fallback output, and dev formatter/linter plugin fetch failures remain in the initial raw runlog sections. No broad OTP-matrix or aggregate alias pass is claimed.

The exact intended planning commit contains eight paths only:

- `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/2026.10.31-audit-results-erlang-date.md`
- `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/date-probes.escript`
- `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/date-probes.out`
- `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/fractional-unit-probes-iteration01.escript`
- `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/fractional-unit-probes-iteration01.out`
- `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/runlog.md`
- `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/closing-report.md`
- `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/ledger.md`

No prompt, plan, CDC verification, source, test, build configuration, or generated beam is included. The initial four-artifact packet and this iteration update remain connected by the candidate history. **CC verdict:** S01-01 through S01-05 are proposed-done and attested, not independently accepted. S01-06 stays open for CDC's final review and retrospective; the slice returns for review and is not advanced.

## Refinement02 close update — 2026-10-31

This narrow wording update executes `cc-prompt-iteration02.md`, issued at planning review commit `c0249ecb58e088eb3463f1ccb6f40c2959066379` after CDC's review of candidate `6054bad49bab9d020f02c954d8d877fa5e186330`. It changes only two factual characterizations in the current report. The prior six-row walk and refinement01 evidence remain preserved above; the table below carries forward all row dispositions without reopening them.

| ID | Refinement02 disposition | Evidence and current scope | Remaining reviewer work |
|---|---|---|---|
| S01-01 | done — CC-proposed, attested | Original full-read manifest inventory and source-cited contract readback remain in `artifacts/runlog.md`; current boundary is recorded in the refinement02 addendum | CDC final check. |
| S01-02 | done — CC-proposed, attested | Five findings and scale coverage remain unchanged; report now correctly records the first-party `scripts/check_coverage.escript` as mapped to Arc02 and unaudited in this slice | CDC final scope review. |
| S01-03 | done — CC-proposed, attested | Finding content/count, scales, probes and controls are unchanged; DATE002 now distinguishes negative-year documentation from the source/history `Y < 1` offset condition | CDC final wording/source check. |
| S01-04 | done — CC-proposed, attested | Original CC and independent CDC OTP28 gate receipts remain pinned; no native gate rerun was warranted for wording-only corrections | Existing plugin-fetch and OTP-matrix limits remain. |
| S01-05 | done — CC-proposed, attested | Source remains unchanged at `bcfd94a`; this refinement's intended commit contains only report, close report, runlog, and ledger | CDC verifies exact diff and source boundary. |
| S01-06 | open — CC close/bubble-up attested; reviewer gate pending | This dated update and preserved row walk; the slice remains proposed, not accepted or advanced | CDC final independent verification and retrospective. |

The executable-scale statement now separates the passive synchronous date library from the repository's tracked, first-party Erlang coverage executable. The project map explicitly assigns the script's complete executable/coverage-contract audit to Arc02; no script analysis is claimed in Slice01. The DATE002 statement now says that README/EDoc describe negative-year restrictions, with README defining year zero as 1 BCE but not separately specifying its offset behavior. Source `apply_offset/4` handles every year `Y < 1`, and archived design-history evidence supports intentional rejection of nonzero offsets for `year <= 0`; this remains excluded from findings. ERL-DATE-002 still concerns invalid calendar month/day tuples accepted on the zero-rounded-offset branch. The five-finding report, probes, controls, severity, and scales are unchanged.

No native gates or public probes were rerun. Initial and iteration01 evidence, their pins, and all exact failure receipts remain intact. `git diff --check` and source no-diff checks are the only new validation. The current exact planning commit is limited to:

- `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/2026.10.31-audit-results-erlang-date.md`
- `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/runlog.md`
- `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/closing-report.md`
- `project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/ledger.md`

**CC verdict:** S01-01 through S01-05 remain CC-proposed-done/attested. S01-06 stays open for CDC's final review and retrospective. This wording update does not constitute slice acceptance.
