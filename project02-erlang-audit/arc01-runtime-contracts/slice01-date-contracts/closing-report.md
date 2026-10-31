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
