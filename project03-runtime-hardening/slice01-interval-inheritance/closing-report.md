# Slice01 CC close — interval inheritance repair

## Public PR delivery and hosted CI result

[PR #76](https://github.com/erlsci/iso8601/pull/76) is OPEN, ready for review, against `main`, at reviewed source `7e93c5d89cb8b14e7a2f5f7a0219201ee1bcfc38`. The source branch is pushed and attached to this chat. [CI run 41260496064](https://github.com/erlsci/iso8601/actions/runs/41260496064) completed with **8/8 jobs successful**, final job completion `2026-11-01T01:19:30Z` (2026-10-31 locally). A final API check confirmed matching PR/run head hashes, all job conclusions, every compile/xref/Dialyzer step successful, and EUnit/PropEr/coverage successful on OTP21–27. OTP20's three test/coverage steps are explicitly skipped by the existing workflow; they are not reported as passed. PR merge state is CLEAN; it remains unmerged.

| Hosted job | Terminal result |
|---|---|
+| [Erlang 20 build (Debian container)](https://github.com/erlsci/iso8601/actions/runs/41260496064/job/124712580641) | success |
| [Erlang 21 build (Debian container)](https://github.com/erlsci/iso8601/actions/runs/41260496064/job/124712580471) | success |
| [Erlang 22 build (Debian container)](https://github.com/erlsci/iso8601/actions/runs/41260496064/job/124712580523) | success |
| [Erlang 23 build (Debian container)](https://github.com/erlsci/iso8601/actions/runs/41260496064/job/124712580515) | success |
| [Erlang 24 build](https://github.com/erlsci/iso8601/actions/runs/41260496064/job/124712580552) | success |
| [Erlang 25 build](https://github.com/erlsci/iso8601/actions/runs/41260496064/job/124712580336) | success |
| [Erlang 26 build](https://github.com/erlsci/iso8601/actions/runs/41260496064/job/124712580511) | success |
| [Erlang 27 build](https://github.com/erlsci/iso8601/actions/runs/41260496064/job/124712580477) | success |

Evidence was inspected with `gh run view 41260496064 --repo erlsci/iso8601 --json databaseId,url,status,conclusion,headSha,event,jobs` and `gh pr view 76 --repo erlsci/iso8601 --json url,title,state,isDraft,baseRefName,headRefOid,mergeStateStatus,files`, both exit0, after `gh run watch ... --exit-status --interval 30` exited0. This supersedes the initially queued CI observation recorded later in the chronological history. Local OTP28 evidence remains additional; OTP28/29 CI expansion and R15B03 proof remain pending. No source correction was required for this run. Seven runtime repairs, human merge/continuation and project completion remain open; the orphan planning branch stays local.

**Current checkpoint: independently verified at source `7e93c5d`; Operator review/merge remains open.** Earlier CC and CDC stages are preserved below; the final parent composition entry supplies the current integrated review verdict.

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
| ERL-DATE-005 | Still applicable: seven of ten fractional-hour/minute comparisons miss the expected subsecond tolerance, including `expanded_hour_fraction`; the whole-second and two explicit-second controls pass. Open for later increment. |

The refinement only adds three post-red classifier edge tests—valid signed year-only full endpoint and empty/malformed signed endpoint rejection—after the coverage report showed those branches were otherwise unexercised. They are part of the final green suite, not claimed as red cases. Coverage failures/retries and exact provider versions are retained in the runlog.

## Ledger and handoff

Slice ledger rows S01-01 through S01-07 are marked `CC-proposed-done` with their evidence paths. S01-08 remains open for CDC's independent verification/retrospective and close. Project03 ledger rows P03-01/02 remain open pending independent verification; all other seven finding rows remain open. Parent-composition P03-10 and Operator P03-11 remain open, as do project acceptance/closure obligations. No plan or reviewer record was changed. The source diff remains within the accepted CAL003/CAL004 scope; no new finding or next increment is proposed.

See `artifacts/runlog.md` for corrected intake extents, exact command attempts/statuses, artifact inventory, source-cited readback and detailed replay dispositions. Stop here for CDC review and parent/Operator checkpoint.

## Refinement01 — formatter regression and receipt correction

Current source candidate is commit `7e93c5d89cb8b14e7a2f5f7a0219201ee1bcfc38` on the same source branch. This refinement changes only `test/iso8601_tests.erl` and the four affected README prompt numbers; production remains byte-identical to initial candidate `ec665b8d7fd554969016f6e5014844d6108c98d0`, SHA-256 `39487e5e94afc299a091b4b41b023b13bdd53e4c0734319a90fdeb8a3d016ebc`. The formatter regression now obtains its start by calling `format_expanded/1`, asserts `<<"+2007-02-15T00:00:00Z">>`, appends `/03-14`, and asserts the independent complete interval tuple with end year2007 before checking semantic interval formatting/parsing.

The isolated corrected EUnit case against unchanged baseline `bcfd94a` exits1 as required: expected endyear2007, actual endyear200. Receipt: `artifacts/r1-baseline-eunit.log`. Fresh full candidate `rebar3 eunit -c -v` exits0 with238 passed (`artifacts/r1-candidate-eunit.log`). Due to the preserved stale cover-only attempt, PropEr was also refreshed before coverage: `rebar3 proper -c` exits0, four properties ×100 cases (`r1-candidate-proper.log`); the following `rebar3 as test cover -v --min_coverage=100` exits0 with module/total100% (`r1-candidate-cover.log`). Fresh `rebar3 as dev fmt --check` and `rebar3 as dev lint` both exit0 (`r1-candidate-fmt.log`, `r1-candidate-lint.log`). These replace no production/native/independent replay receipts, which remain valid at unchanged `ec665b8` production. README prompt numbering now continues22–25; the preceding duplicate5/6 history is untouched.

The initial close `3908b5a` reported six DATE005 failures. The complete preserved fractional replay was re-read and shows seven false comparisons and three true controls; this current report supersedes that count, with `expanded_hour_fraction` among the seven. The replay log and original close remain unchanged. The prior runlog omitted the triggered coverage-guide receipt; the refinement runlog now preserves both the historical timing disclosure and the full guide's reread extent, plus exact known nonfinal attempt commands, exits, and log paths. S01-05's evidence path is the sibling `closing-report.md` (not the nonexistent `artifacts/closing-report.md`).

S01-08, CDC acceptance/retrospective, parent composition, project rows P03-10/P03-11, and Operator checkpoint remain open. No next increment or external action was taken.


## Current CDC disposition — first checkpoint review-ready

CDC independently accepts Slice01 at final source `7e93c5d89cb8b14e7a2f5f7a0219201ee1bcfc38`; see [verification](cdc-verification.md) for all8 slice rows/R1–R4 and [retrospective](cdc-retrospective.md) for checkpoint learning. Earlier CC proposed-done statements above are preserved historical stages. CAL003/CAL004 are repaired within the binding scope; all seven other finding rows remain open. Final affected EUnit238,100%coverage,fmt/lint independently pass; actual formatter regression independently fails unchanged baseline. Earlier independently reproduced static/property gates and147 applicability observations retain the same production SHA, without being called fresh after the final test/doc-only revision.

Current proof is local OTP28, not supportedOTP20–27/R15B03 or hosted CI/aggregate Make/release acceptance. Current complete coverage-guide reload is supported, but original historical timing is CC attestation; failed cover-only89% followed by refreshed100% is retained without claiming a proven stale-input cause. Parent independent composition P03-10 and Operator review/merge P03-11 remain open; project completion remains open. No next increment, push, PR, merge or release occurred. Parent owns the next composition entry after CDC's committed handoff.

## Parent independent checkpoint composition — 2026-10-31

Parent reviewed CDC assembly `c84a55e6548952f681852b1fa6edd59b85abc26f`, including the complete production/test/README diffs, final correction, slice ledger, independent verification and retrospective. **P03-10 is done/reproduced.** CAL003/CAL004 compose into the promised interval repair candidate. P03-11 remains an explicit human review/merge/continuation decision; this is neither a full-project close nor main integration.

Final source is `7e93c5d89cb8b14e7a2f5f7a0219201ee1bcfc38`, tree `90b1466fabc482ef45debc846bcd0657db625153`, production SHA-256 `39487e5e94afc299a091b4b41b023b13bdd53e4c0734319a90fdeb8a3d016ebc`. Its tree is exactly equal to `9227a20`, against which parent independently compiled and ran the final test module; the subsequent amend changed only commit attribution metadata. The production hash also equals the earlier `ec665b8` tested module. Those explicit identities justify retaining applicable parent/CDC observations without calling them new executions at the replacement commit.

Parent independently prepared [19 exact-value/rejection controls](artifacts/parent-composition-controls.escript) before implementation. Fresh `bcfd94a` production produced 10 repair failures and nine passing controls, exit1. A separately fresh candidate beam produced 19/19 passes, exit0. Cases include signed short/padded/large years, signed complete ends, positive/negative inherited zones, explicit Z/opposing-zone overrides, raw local date/year crossings, fractions, minute-only ends, and retained unsigned/unzoned/malformed/negative-year exclusions. The same fixture covers before/after observations; it is retained here so this independent composition check can be reproduced without its temporary directory.

Observed commands used `/private/tmp/iso8601-hardening-parent-exwnwjh2/ebin` as the isolated output path: `erlc -o <ebin> src/iso8601.erl`, then `escript <fixture> <ebin>`. The first compile ran from main at `bcfd94a`; the second from the source candidate worktree at `ec665b8`. Both compiles exited0. Candidate stdout was captured at `/private/tmp/iso8601-hardening-parent-exwnwjh2/candidate-ec665b8.out`; original baseline stdout was observed directly in the tool result, not saved as a separate file. The committed fixture accepts its ebin path as an argument and has no fixed worktree dependency.

Parent separately called `format_expanded({{2007,2,15},{0,0,0}})`, appended `/03-14` to the actual binary, and checked the full expected interval with both years2007: pass. Final permanent regression now exercises this producer path and independently specifies the expected tuple, rather than comparing two parser results. Parent freshly compiled final `test/iso8601_tests.erl` into its isolated ebin and ran `case eunit:test(iso8601_tests,[verbose]) of ok -> halt(0); _ -> halt(1) end`: **238 passed**, exit0. Full observed output is `/private/tmp/iso8601-hardening-parent-exwnwjh2/parent-eunit-9227a20.log`. The corrected README continuation and its exact values agree with the exercised APIs.

Parent used `epp:parse_file/3` to compare sorted `export` and `export_type` attributes against main `bcfd94a`: all15 function name/arity pairs and11 exported type pairs are exactly equal. Actual diff contains only module/test/README paths; `git diff --check bcfd94a` passed. All three reachable source commits have contiguous required coauthors. Project01 is unchanged since `213bfc7`; both original Project02 audit-slice subtrees are unchanged since `cfbf539`. The later count corrections do not alter those original payloads.

CDC independently reproduced the eight native gates, final affected EUnit238/100%cover/fmt/lint, actual formatter-focused baseline failure and all147 original observations (31+10+42+64). Parent checked the corresponding disposition and retained-versus-fresh boundaries rather than relabeling CDC commands as its own. Only CAL003/CAL004 are repaired; CAL001/CAL002/DATE001–005 remain applicable and open. Coverage/read-timing/failed-attempt qualifications in the CDC review and retrospective remain binding. This evidence establishes a local OTP28 candidate, with OTP20–27/R15B03/hosted CI and requested OTP28/29 CI work still unverified or pending as specified in Project02. No next repair increment, push, merge, PR or release occurred.

## Requested public pull request — 2026-10-31

The Operator explicitly requested a PR for this publicly consumed library. Source candidate `7e93c5d89cb8b14e7a2f5f7a0219201ee1bcfc38` was pushed to GitHub origin as `codex/project03-arc01-intervals`; [PR #76 — Fix expanded-year and timezone inheritance in intervals](https://github.com/erlsci/iso8601/pull/76) is open, ready for review, against `main`. The PR is attached to this Codex chat. Remote base was refreshed at `c594869`; the public diff consequently includes the earlier authorized `AGENTS.md`/`CLAUDE.md` guidance cleanup as well as module/tests/README changes. There is no base drift and no source payload change from the independently accepted candidate.

Creation triggered [GitHub CI run 41260496064](https://github.com/erlsci/iso8601/actions/runs/41260496064), eight OTP20–27 jobs, initially all queued. This is an observed initial state, not a CI pass; append the terminal result below after monitoring. The configured OTP20 tier omits EUnit/PropEr/coverage, as already documented. OTP28/29 CI expansion remains a separate Project02 obligation. The planning branch stays local. P03-11 merge/continuation is still open; opening this PR does not merge, release or authorize the next increment.
