# Slice03 CC close candidate — OTP28/29 CI

**Current disposition: CC-proposed-done for the bounded local refinement; the slice remains open.** The final-hosted-run and independent-review rows are still open. This return is not CDC acceptance, parent composition, Operator acceptance, or a published change.

## Refinement01 local return — 2026-10-31

Source commit `845b1830b5acbfaed0909430e1ff011617a06817` (tree `084a53ea1aa64cb578dbccd5a609abdaf23ac220`) changes only the private spec for `apply_years_offset/2` from `number()` to `integer()`, matching the actual OTP29 Dialyzer success type and both caller contracts. CDC had independently reproduced the sole blocker in hosted run41261632042: OTP29.1.2/rebar3.28.0 compile/xref pass, Dialyzer fails one underspec, and EUnit/PropEr/coverage then skip. The previous9/10 run remains a failed acceptance result.

The one-off OTP28 AST comparison removed only that spec attribute from each normalized parsed form set; all remaining parsed forms were equal. A separate comparison found all15 exports,11 exported types and public specs unchanged. The focused existing timestamp/year/duration/leap-clamp tests remain in place. Fresh sequential local gates all passed on OTP28.1.1/rebar3.27.0: compile, xref, Dialyzer, EUnit238, PropEr4×100,100% coverage, format check and lint. Exact commands/output/exits are in [`artifacts/refinement01-native.log`](artifacts/refinement01-native.log); full intake, preservation comparison and limits are in [`artifacts/runlog.md`](artifacts/runlog.md).

This is not OTP29 validation. No global OTP29 install or runtime replay/hang probe was performed. The final ten-job hosted rerun on the parent-published final PR head remains mandatory; actual OTP29 Dialyzer and its tests must run successfully before S03-05 can close. S03-06 also remains open for CDC independent disposition/retrospective and the separate parent/Operator checkpoints.

**Disposition: CC-proposed-done for the locally deliverable candidate; the slice remains open.** The exact-hosted-run and independent-review rows are still open. This is an implementation return to CDC, not CDC acceptance, parent composition, Operator acceptance, or a published change.

## Initial assignment return — before hosted run41261632042

Executed initial `cc-prompt.md` at planning open-set commit `14d1316596937b61dead8bc0511f07699f85bdaa`, under parent priority/merge commit `cb672b23197cf3e0b36e820f57ee27b0f6c718c`. Source branch `codex/project02-otp-28-29` began clean at `1d85d0a98a21750f83355a855dcc8fb4728758b4`, tree `90b1466fabc482ef45debc846bcd0657db625153`.

Source candidate commit `fcc1fe22e1651e133e20b7eb798dece82969e348`, tree `24abd4af8e85d8f9ed5805487217100dad3ddb41`. The commit contains exactly `.github/workflows/ci.yml` and `AGENTS.md`. It adds OTP28/29 to the Ubuntu22.04/setup-beam tier, selects exact rebar3 3.28.0 at 28/29 while retaining existing selections, and updates the two AGENTS configured CI/test ranges. OTP20–27's matrix placement, commands, skip conditions, runner/actions, fail-fast behavior, triggers, and coverage floor are retained.

The complete production, EUnit, PropEr, rebar configuration, and app-manifest population remains byte-identical to intake. Its five SHA-256 values and the source-cited design/readback are in [`artifacts/runlog.md`](artifacts/runlog.md). No compatibility blocker was reproduced, no source/test/build mutation was needed, and no 147-observation replay was rerun for YAML/guidance-only changes. `CLAUDE.md` remains its `AGENTS.md` symlink. No application version or README change was made.

## Initial validation and evidence limits

Fresh local `rebar3 compile` and `rebar3 xref` each exited 0 on OTP28 / ERTS16.1.1 / rebar3 3.27.0. A corrected PyYAML `BaseLoader` assertion exited 0 after confirming exactly ten majors20–29, tier membership, OTP20-only test skips, six native commands on21–29, toolchain selectors, runner/actions, `fail-fast:false`, triggers and100% coverage. `git diff --check` exited 0. One initial local assertion exited1 because `BaseLoader` returned strings and the assertion expected integers; the corrected assertion passed. Full command/observation details and the unchanged-source evidence pins are retained in the [runlog](artifacts/runlog.md).

The runlog retains CDC's independent eight native gates at the unchanged production SHA and the prior PR76 8/8 OTP20–27 result. They are not fresh candidate runs and do not validate OTP28/29. At that initial return, no ten-job hosted result existed. Parent later published this candidate as PR77; run41261632042 finished9/10 and exposed the one OTP29 Dialyzer blocker documented above. S03-05 remains open until a reviewed correction is published and all ten exact-head jobs pass. The initial local OTP28 receipt was supplemental and did not establish OTP29 compatibility.

## Initial ledger walk — status at the first CC return

| Row | CC disposition | Evidence / remaining gate |
|---|---|---|
| S03-01 | CC-proposed-done | Source/planning branch identities, clean intake, model/ownership authority, required read extents, and source-cited contract readback are recorded in the [runlog](artifacts/runlog.md). CDC independently verifies the intake and pins. |
| S03-02 | CC-proposed-done | Source commit `fcc1fe2`; exact workflow diff and parsed structural/content assertions are in the runlog. The actual hosted compatibility criterion is S03-05 and remains open. |
| S03-03 | CC-proposed-done | AGENTS range-only update; hashes and no-drift check establish no executable/API/type change. No blocker or compatibility fix occurred. CDC checks the scope and public contract. |
| S03-04 | CC-proposed-done | Fresh compile/xref plus retained, independently reproduced unchanged-source gates are pinned in the runlog. No broad local gate suite was rerun for the YAML/guidance-only candidate. CDC checks applicability and receipts. |
| S03-05 | Open | No ten-job run on this candidate. Parent owns publication and inspection of the actual10 job results. |
| S03-06 | Open | CDC must independently review/reproduce the candidate, disposition all rows and write the required retrospective. Parent composition and Operator merge decision are separate gates. |

The ledger retains all six opening rows. CC's proposed statuses do not close them independently.

## Initial artifact inventory

Supporting evidence under `artifacts/` comprises the continuing [`runlog.md`](artifacts/runlog.md), preserved initial hosted failure records, and [`refinement01-native.log`](artifacts/refinement01-native.log) for the fresh local gates. This `closing-report.md` and the existing slice documents remain first-class files at the slice root; no verifier or test product was created.

## Initial bubble-up disposition — before the hosted failure

This slice is the direct child of Project02 under the recorded one-slice collapse; there is no arc wrapper or `arc-plan.md`. The implementation piece is a committed, reviewable workflow candidate that will schedule OTP28/29 checks using the supported selector. Its integration state is local and unpushed. The local configuration check confirms the ten intended jobs are described correctly, while actual usability remains contingent on the parent-owned ten-job hosted run. Do not treat this candidate as integrated OTP support yet.

No unanticipated dependency or scope change emerged. The initial BaseLoader assertion mistake was corrected and recorded; it does not change the plan or acceptance criteria. CDC should independently confirm that no project-plan amendment is needed before later work uses the expanded matrix.

Scope-as-specified versus scope-as-delivered: both authorized source paths changed as planned; OTP20–27, the R15B03 intended floor, public API/types, runtime code, tests, dependencies, README, and release version were preserved. The full-ten-job hosted result, independent CDC review, audit applicability check, retrospective, parent publication/composition and Operator merge remain explicitly outstanding. No other specified item was silently dropped.

**Planning close-packet commit:** pending until this report, ledger and runlog updates are committed on the local orphan `planning` branch; the handoff will identify the exact resulting commit. No source push, PR, merge, hosted run, release, or next slice was initiated by CC.


## CDC refinement01 prereview / parent rerun handoff

Independent [review](cdc-verification.md) supports source845b183 for publication: one private annotation corrected, every other parsed form equal to the prior candidate, fresh all8 native gates0 (238EUnit,4×100PropEr,100%coverage). CC returnff7edade remains proposed local completion; actual final-head10-job hosted regression and final close/retrospective remain open. The first9/10 run is preserved failure evidence, not silently replaced. No additional runtime finding repaired, no full147 replay claimed, no merge/release. Parent owns the next reviewed source push and hosted run.


## Refinement02 local candidate return — 2026-10-31

The second actual hosted run on `845b183` had9/10 successful jobs; OTP29 compile/xref/Dialyzer passed, but EUnit had236 passes and two failures. The preserved job JSON/log identify invalid positive date `2017-02-30` being normalized to March2 and negative week `-0001-W01-1` being accepted. The second refinement changes only `make_date/4` input boundaries and the matching EUnit assertions/controls: positive-year raw calendar dates are checked with `calendar:valid_date/1` before normalization, and negative week dates raise `badarg` after the calendar-date clause. Negative plain dates, year-zero weeks and positive week normalization remain covered. The OTP28 baseline incidental reasons (`if_clause` and `function_clause`) were captured and are deliberately stabilized to `badarg`; this is an error-reason clarification, not a claim those older reasons were already `badarg`.

Source candidate commit `3071050f501a9651356061d5c9da859cb9da862b`, tree `3224b1e0da9285ab67349aadc3b59ffa11bfe8ab`, is ready for review; it contains only `src/iso8601.erl` and `test/iso8601_tests.erl`. The EPP public export/type/spec projection is identical to baseline. Fresh sequential local OTP28 gates all exit0: compile, xref, Dialyzer, EUnit249, PropEr4×100, coverage100%, format check, lint. A fresh isolated ebin replayed all147 retained date/fractional/calendar observations; only the intended invalid-positive-date error class changed. Full commands, readback, replay comparison and current limits are in [`artifacts/runlog.md`](artifacts/runlog.md); native/replay logs are inventoried there.

CC proposes S03-01, S03-02, S03-03 and S03-04 done/reproduced. S03-05 stays open: the parent must publish the reviewed final head and obtain all10 hosted jobs with OTP29 EUnit, PropEr and coverage executed successfully. S03-06 stays open for independent CDC review of this candidate, the retrospective, parent composition and Operator checkpoint. No global OTP29 install, push, PR, merge, release or broadened runtime repair was performed. Other audit findings, including DATE001 week-range behavior and DATE002 nonpositive plain-date validity, remain open.

**Refinement02 source candidate commit:** `3071050f501a9651356061d5c9da859cb9da862b` (tree `3224b1e0da9285ab67349aadc3b59ffa11bfe8ab`). **Planning close-packet commit:** pending explicit-path commit on the local `planning` branch.
