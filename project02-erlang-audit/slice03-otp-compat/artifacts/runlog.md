# Slice03 CC runlog — OTP28/29 CI

## Intake before edits — 2026-10-31

### Source and planning identity

- Source: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-otp-28-29`, branch `codex/project02-otp-28-29`, clean at `1d85d0a98a21750f83355a855dcc8fb4728758b4`; tree `90b1466fabc482ef45debc846bcd0657db625153`.
- Planning: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/planning`, branch `planning`, clean at `14d1316596937b61dead8bc0511f07699f85bdaa`.
- Worktree list inspected before mutations. Main checkout remains `main` at `1d85d0a`; no source or planning changes existed at intake. Source checkout contained ignored `_build/`, inherited from CDC's rebar3 preflight; it is not treated as a validation baseline and was not cleaned.
- Local tools: `/usr/local/bin/rebar3`; `rebar3 --version` exit 0: `rebar 3.27.0 on Erlang/OTP 28 Erts 16.1.1`. Python PyYAML version check exit 0: `6.0.3`.

### Required-reading manifest and actual extents

Line counts were read from the actual files. Full reads were performed in bounded, untruncated chunks; section reads are identified by line range. A receipt records visibility and scope, not independent acceptance.

| Requirement | Source pin / actual extent | Disposition |
|---|---|---|
| Source `AGENTS.md` | Handoff source tree `1d85d0a`, 110/110 lines | Full. Compatibility, API, validation, local-commit and external-action constraints govern. |
| Planning `AGENTS.md` | Planning `14d1316`, 16/16 lines | Full. Orphan planning branch, canonical artifacts, preservation and commit trailers govern. |
| Initial prompt, slice plan, slice ledger | Planning `14d1316`; `cc-prompt.md` 37/37, `slice-plan.md` 53/53, `ledger.md` 12/12 lines | Full. Prompt is active; six rows remain the acceptance contract. |
| Parent project plan and ledger | Planning `14d1316`; `project-plan.md` 110/110 and `ledger.md` 10/10 lines | Full. Read outcome/boundaries, roadmap, dated CI priority/merge authority/workflow, P02-06 and open project gates. |
| `collaboration-framework/SKILL.md` | Installed skill entrypoint, 574/574 lines | Full in chunks 1–190, 191–400, 401–574. Autonomous role separation, intake, evidence and closure apply. |
| `erlang-guidelines/SKILL.md` | Installed skill entrypoint, 284/284 lines | Full in chunks 1–160 and 161–284. OTP-27 defaults explicitly yield to repository compatibility. |
| `work-verification/SKILL.md` | Installed skill entrypoint, 51/51 lines | Full. Proposed-done remains distinct from independent review. |
| `testing/SKILL.md` | Installed skill entrypoint, 35/35 lines | Full. Repository-native gates govern. |
| `engineering-methods/guides/01-engineering-methodology.md` | Installed skill, 677 lines; read Notes for Codex / role adapter and shared invariants at 1–154, Two-Contributor responsibilities/authority/handoffs/verification/closure at 61–153, Autonomous Mode incl. mandatory model selection and stable preflight at 326–550 | Required sections read. Operator's selected CC/CDC contexts and existing scope govern; no external action authority transfers to CC. |
| `project-management/guides/README.md` | Installed skill, 232 lines; read 1–230, including Notes for Codex, required load set and Autonomous Mode | Required sections read; the final two lines were not needed for this manifest. Existing slice open set and `artifacts/` home are preserved. |
| `project-management/guides/02-canonical-planning-worktree.md` | Installed skill, 334/334 lines | Full. One-slice collapse, document/artifact home, preservation, verification and iteration rules apply. |
| `project-management/guides/03-planning-top-down.md` | Installed skill, 341 lines; read planning authority/usable increment/investigation sections 1–78, per-slice open set and implementation-ready assignment/intake/iteration sections 149–256 | Required sections read. Issued prompt remains immutable; no new scope is inferred. |
| `work-verification/guides/03-row-closure.md` | Installed skill, 81/81 lines | Full. Every row will be walked; CC dispositions are attested/proposed-done only. |
| `testing/guides/01-testing-discipline.md` | Installed skill, 91/91 lines | Full. No test was added to mirror the matrix; failures remain visible. |
| `testing/guides/03-validation-gates.md` | Installed skill, 132/132 lines | Full. Proportionate structural validation plus fresh required local gates; hosted execution remains separate. |
| `erlang-guidelines/guides/11-anti-patterns.md` | Installed skill, 571/571 lines, chunks 1–200, 201–400, 401–571 | Full, first as required. AP-06 supports preserving failure visibility. No Erlang changes are planned. |
| `erlang-guidelines/guides/17-tooling.md` | Installed skill, 317/317 lines, chunks 1–180 and 181–317 | Full, after anti-patterns. TL-01/04/07/09 support repository-native compile/xref and hosted gates; TL-05/06/08 do not add gates or policy changes beyond this assignment. |
| Workflow, `rebar.config`, `Makefile`, app manifest | Handoff source tree `1d85d0a`; respectively 92/92, 89/89, 96/96, 21/21 lines | Full. Commands, OTP tiers, profiles, coverage and trigger contract inspected. |
| README | Handoff source tree `1d85d0a`; About/New-in-1.4 sections (lines 21–48) and Erlang support badge definition (line 250) | Required sections. Existing `>=20` badge remains true after matrix expansion. |
| Source API/type declarations | Handoff source tree `1d85d0a`; `src/iso8601.erl` lines 1–55 of 824 | Required section. 15 public functions and 11 exported types; stable API and older floor remain untouched. |
| PropEr contract | Handoff source tree `1d85d0a`; `test/prop_iso8601.erl` 111/111 lines | Full. Four properties; no test changes in scope. |
| Existing Project03 native evidence | Planning `14d1316`; Project03 CDC verification final disposition and `artifacts/cdc-validation.log` command receipts; source `7e93c5d` / same tree `90b1466`, production SHA-256 `39487e5e94afc299a091b4b41b023b13bdd53e4c0734319a90fdeb8a3d016ebc` | Read applicable verification passages. CDC independently reports the eight sequential gates compile, xref, Dialyzer, EUnit (238), PropEr (4×100), 100% coverage, format, lint successful at that production pin; these are retained unchanged-source evidence, not fresh runs here. |
| Existing PR76 hosted evidence | Planning `14d1316`; Project03 slice close's public-PR/CI result; run `41260496064`, head `7e93c5d`, source tree `90b1466` | Read applicable result/step disposition. Historical result is 8/8 OTP20–27 jobs; OTP20's test trio skipped and 21–27 test trio successful. This does not validate OTP28/29. |
| Setup-beam and rebar3 primary documentation | Direct current reads on 2026-10-31: setup-beam README lines 183–224 and action `v1/action.yml` lines 22–25, 50–58, 78–95; rebar3 README lines 49–67; release 3.28.0 lines 158–165 | Required sections read directly. Ubuntu22.04 lists OTP24.2–29; setup-beam accepts exact/range strings, defaults loose matching, and exposes exact installed OTP/rebar3 versions. Rebar table supports3.28.0 on27–29 and3.25.1 on26–28. Release notes describe Certifi removal in favor of the OS root store and a vendored `hex_core` security bump. These primary docs substantiate the selector, while hosted resolution remains unverified. |

### Source-cited contract readback

The active [slice plan](../slice-plan.md), grounded in the workflow and primary documentation it cites, requires an exact ten-major matrix, preserving the four `older-builds` container jobs for OTP20–23 and extending the `newer-builds` list from 24–27 to 24–29. The existing `if: matrix.otp_version >= 21` conditions make OTP20 the sole test-skip row; its compile, xref and Dialyzer steps remain. Jobs21–29 retain all six native commands (`compile`, `xref`, `dialyzer`, EUnit, PropEr, 100%-coverage), Ubuntu22.04, checkoutv6, setup-beamv1, `fail-fast: false`, push/PR triggers and the coverage threshold. The selector must use exact rebar3 3.28.0 for 28/29, while 26/27 retain 3.25 and 24/25 retain 3.22; old jobs remain on manual checkout/rebar3 3.15.2.

The source [AGENTS.md](../../../../../AGENTS.md) currently states the old configured ranges and is the only documentation target: change its CI/test ranges to 20–29/21–29, preserving R15B03 as intended but not demonstrated and every other instruction. The README's `>=20` badge remains accurate. The source workflow [`.github/workflows/ci.yml`](../../../../../.github/workflows/ci.yml), rebar profile [configuration](../../../../../rebar.config), app manifest and code declarations establish no need to change application version, dependencies, syntax, test populations, or public exports/types. The repository's 100% CI coverage requirement stays distinct from Make's 95% executable-line script.

Direct primary-source check: [setup-beam README](https://github.com/erlef/setup-beam/blob/main/README.md#input-versioning) recommends string versions and documents range resolution, its [OS compatibility table](https://github.com/erlef/setup-beam/blob/main/README.md#compatibility-between-operating-system-and-erlangotp) lists Ubuntu22.04 with OTP24.2–29, and the [`v1` action contract](https://raw.githubusercontent.com/erlef/setup-beam/v1/action.yml) defines range inputs, loose matching and exact resolved version outputs. The [rebar3 compatibility table](https://raw.githubusercontent.com/erlang/rebar3/main/README.md) explicitly pairs3.28.0 with27–29 and3.25.1 with26–28; the [3.28.0 release notes](https://github.com/erlang/rebar3/releases/tag/3.28.0) identify the root-store/Certifi and hex_core changes. These are mutable main/v1 documents and support the recipe only; actual hosted version resolution remains a gate.

The allowed implementation paths are only `.github/workflows/ci.yml` and `AGENTS.md`. Source/tests/rebar fixes require a reproduced 28/29 blocker and a CDC-approved numbered refinement. The source tree's app manifest, module, EUnit and PropEr files match the reviewed Project03 candidate at the pinned tree/hash. Thus the retained native evidence remains applicable to unchanged production source, while the new hosted ten-job run is still mandatory and cannot be substituted by local OTP28 or the prior eight-job run. CC's local close can be proposed-done only; CDC review, hosted result and Operator merge gate remain open.

### Baseline executable/build/test identity

Targeted `git diff --binary 1d85d0a98a21750f83355a855dcc8fb4728758b4 -- src test rebar.config` was empty (0 bytes); `git diff --name-status` returned no paths. The complete tracked target population is five files: `rebar.config`, `src/iso8601.app.src`, `src/iso8601.erl`, `test/iso8601_tests.erl`, `test/prop_iso8601.erl`. SHA-256 at intake:

| Path | SHA-256 |
|---|---|
| `src/iso8601.app.src` | `3c1d9d00c0d444c11e1043969b8b59305aeee640b167028ea0698876d55dacf1` |
| `src/iso8601.erl` | `39487e5e94afc299a091b4b41b023b13bdd53e4c0734319a90fdeb8a3d016ebc` |
| `test/iso8601_tests.erl` | `ae801a4fd3c32aa09a9041aa0b989f4c34d711589c04a3c6bed6c541dc6d02c8` |
| `test/prop_iso8601.erl` | `7a0d39b64438241599ae6017508d5b415a75cf82cbcd712da025db3816e2e17a` |
| `rebar.config` | `39e573f074525a6d79381cb589f09bf42fa55398121a6951b6182274a4c6e513` |

No dependency was added, no runtime compatibility failure was reproduced, and no conditional source/test/build read or mutation was triggered.

## Candidate implementation and checks

### Scoped source change

Source commit `fcc1fe22e1651e133e20b7eb798dece82969e348`; tree `24abd4af8e85d8f9ed5805487217100dad3ddb41`; branch `codex/project02-otp-28-29`. The commit contains only `.github/workflows/ci.yml` and `AGENTS.md` (4 insertions, 4 deletions) and has the required contiguous co-author trailers. `git status --short --branch` is clean. `.gitignore` reports `_build/` and a generated `rebar.lock` as ignored; neither is part of the commit. `CLAUDE.md` remains a symlink to `AGENTS.md`.

The diff adds 28/29 to the newer matrix and puts the `>=28` exact `3.28.0` selector before the unchanged `>=26` `3.25` and fallback `3.22` selectors. Existing older OTP20–23 container setup, manual checkout, and 3.15.2 remain byte-identical. Only the two AGENTS compatibility range statements changed. The complete `src/`, `test/`, `rebar.config`, and app manifest population remains byte-identical to intake; final SHA-256 values equal the table above. There are no production, EUnit, PropEr, rebar config, manifest, dependency, API/type, README, or version edits.

### Exact local checks and outcomes

All commands ran in `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-otp-28-29`; no concurrent rebar command ran.

| Command / observation | Exit | Result and limit |
|---|---:|---|
| `rebar3 --version` | 0 | `rebar 3.27.0 on Erlang/OTP 28 Erts 16.1.1`; supplemental local runtime identity, not a 3.28.0 or OTP29 install. |
| First one-off `python3` / PyYAML BaseLoader matrix assertion | 1 | Assertion expected integer scalars, while BaseLoader deliberately returns strings. No workflow defect or runtime blocker; failure retained and the check corrected. |
| Corrected one-off `python3` YAML structure/content assertions with PyYAML `BaseLoader` | 0 | Preserved `on` key; exact majors20–29 once each; old20–23/new24–29 tiers; only OTP20 test skips; all six commands for21–29; old3.15.2; exact new selector and old lower selections; runner/actions, fail-fast false, triggers, and100% coverage. Output: `PASS YAML BaseLoader structure`; `PASS exact ten majors 20-29, unique, old tier 20-23, new tier 24-29`; `PASS OTP20 only test skips; jobs21-29 retain six native commands`; `PASS old rebar3.15.2 and 24/25 -> 3.22, 26/27 -> 3.25, 28/29 -> 3.28.0`; `PASS runners/actions, fail-fast:false, triggers, six-command coverage floor preserved`. |
| `rebar3 compile` | 0 | Fresh compile on the local OTP28/rebar3.27.0 substrate; compiled `iso8601` without warnings/errors in the output. |
| `rebar3 xref` | 0 | Fresh configured undefined/unused/deprecated-call cross-reference gate on the same local substrate. |
| `git diff --check` | 0 | No whitespace errors. |
| `git diff --stat`, `git diff --name-status`, scoped/full-source diff review and final SHA checks | 0 | Exactly two tracked paths changed; executable/build/test paths and hashes unchanged; no CLAUDE target diff. |

The corrected YAML check used `yaml.load(Path('.github/workflows/ci.yml').read_text(), Loader=yaml.BaseLoader)` so the YAML1.1 `on` key was preserved. It was a one-off assertion, not a tracked test or reusable verifier. The initial assertion failure and corrected successful observation are both retained above.

### Retained evidence and unrun gates

Project03's [CDC verification](../../../project03-runtime-hardening/slice01-interval-inheritance/cdc-verification.md) independently reports all eight native gates successful sequentially at source candidate `7e93c5d89cb8b14e7a2f5f7a0219201ee1bcfc38`, tree `90b1466fabc482ef45debc846bcd0657db625153`, with production SHA-256 `39487e5e94afc299a091b4b41b023b13bdd53e4c0734319a90fdeb8a3d016ebc`: compile, xref, Dialyzer, EUnit238, PropEr4×100, coverage100%, format check and lint. The exact command receipts are in its `artifacts/cdc-validation.log`. These are retained independently reproduced unchanged-production evidence, not fresh executions at this source commit. The parent-reported [PR76 hosted result](../../../project03-runtime-hardening/slice01-interval-inheritance/closing-report.md) is 8/8 OTP20–27 jobs at `7e93c5d`; OTP20's test trio skipped and OTP21–27's test trio succeeded. Neither retained result demonstrates OTP28/29 support.

No fresh Dialyzer, EUnit, PropEr, coverage, formatting, lint, or 147-observation replay was run because only workflow YAML and AGENTS guidance changed and executable/build/test inputs remain identical. The fresh compile and xref supplement rather than replace retained native evidence. No new10-job hosted run exists: S03-05 remains open until the parent publishes the reviewed candidate and observes ten successful jobs, exact resolved versions, step outcomes and test populations. No OTP29 claim, R15B03 validation claim, CI rollup claim, merge, or release is made. CDC independent review, close disposition and retrospective remain pending.

### Close-packet reading extension

Before preparing the close set, read the full installed `project-management/guides/04-closing-slices.md` (122/122 lines). It requires a row-for-row walk, artifact inventory, implementation state and scope comparison, plus a bubble-up decision. This one-slice project increment has no arc wrapper, so the closing report labels the equivalent roll-up to its direct project parent and records that no plan change is proposed; CDC must independently decide whether that disposition holds.

Planning commit first attempt: `git commit --only project02-erlang-audit/slice03-otp-compat/ledger.md project02-erlang-audit/slice03-otp-compat/closing-report.md project02-erlang-audit/slice03-otp-compat/artifacts/runlog.md ...` exited1 because the two new close files were not yet known to Git; Git reported their pathspecs did not match. The attempt changed no files. The exact three owned paths were then explicitly added to the index for the commit retry.

Post-commit scope-check correction: an overly broad `git diff --check 1d85d0a98a21750f83355a855dcc8fb4728758b4..HEAD` against the orphan planning branch exited2 and reported four trailing-whitespace lines in older Project03 validation logs. That source-branch base is not an applicable planning diff base; the reported files are pre-existing, outside this slice's ownership, and were not changed. The correct scoped checks `git diff --check HEAD^ HEAD` and `git show --check HEAD` on the planning close commit both exit0. The source candidate's `git show --check HEAD` also exits0; its status is clean and its commit contains only the two authorized source paths.

## Refinement01 intake — private OTP29 Dialyzer spec — 2026-10-31

### Handoff and preserved hosted failure

- Source checkout: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-otp-28-29`, branch `codex/project02-otp-28-29`, clean before this refinement at `fcc1fe22e1651e133e20b7eb798dece82969e348`, tree `24abd4af8e85d8f9ed5805487217100dad3ddb41`; exact source change point is this prior reviewed candidate.
- Planning checkout: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/planning`, branch `planning`, clean at open-set `509fa0d6cc53781a8c465ba0d5d88f83e70b4f3c`. Refinement authority is `cc-prompt-iteration01.md`; initial `cc-prompt.md` remains immutable. Existing slice plan v1.2 and six-row ledger govern; no new refinement beyond this one.
- Required-full refinement prompt, initial prompt, current slice plan, ledger and CDC verification were opened in bounded output. Initial `closing-report.md` and the intake/candidate sections of `artifacts/runlog.md` were reviewed to preserve earlier receipts, failures and source pins. The initial failed BaseLoader assertion, wrong-base planning diff-check and first commit pathspec failure remain recorded as mistakes, not erased or relabelled as product failures.
- Required-full actual module `src/iso8601.erl` at source `fcc1fe2`: 824/824 lines read in ranges 1–300, 301–560 and 561–824. Required-full API guide `02-api-design.md`: 355/355 lines. Required-full data/types guide `04-data-and-types.md`: 479/479 lines. These guides' OTP27+ defaults yield to this repository's older-floor and EDoc constraints. API-07/DT-16 support an accurate private spec; DT-18 limits what a successful Dialyzer result establishes. Existing anti-pattern11 and tooling17 remain from the initial full intake; no runtime/error/function recipe changed, so no new conditional guide trigger arose.
- Required-section `test/iso8601_tests.erl`: lines 324–520 of 1302, covering timestamp dispatch, integer `add_years/2`, simple and compound year-duration paths, positive/negative year offset, and leap-day clamp. Required-full `test/prop_iso8601.erl`: 111/111 lines; four existing properties, no test edit planned.
- Required-full preserved OTP29 log `artifacts/hosted-first-run-otp29.log`: 282/282 lines. It records run41261632042 at `fcc1fe2`, Ubuntu22.04.5, actual OTP29.1.2/ERTS17.1.1 and rebar3.28.0. Compile and xref succeed; Dialyzer exits1 with one line529 underspec: `apply_years_offset(datetime(),number()) -> datetime()` is broader than its inferred integer second argument. This is the actual blocker; no other library failure was found in this job. Required-data `hosted-first-run-jobs.json` was parsed as a complete ten-job projection:20–28 are successful;29 fails only Dialyzer; the six native gates are successful for20–28 except20's EUnit/PropEr/coverage are deliberately skipped, and OTP29's three test steps are skipped following Dialyzer failure. Run remains9/10, not accepted.
- Required-section `project-management` guide02 iteration/preservation and guide03 issuing/executing sections, plus existing row-closure/testing gates, remain the active procedures from initial full intake. Existing history is preserved; runlog relative Project03 artifact links use `../../../project03-runtime-hardening/...` from this `artifacts/` directory.

### Source-cited contract readback before edit

At source `fcc1fe2`, exported `add_years/2` promises `integer()` at lines82–83 and delegates its unchanged `Y` argument at lines84–87. `parse_duration/1` captures year digits (`[0-9]+`) and `gi/1` calls `string:to_integer/1` at lines169–185; `apply_duration_plist/3` binds the year value and applies only identity or unary negation at lines564–577. Both paths therefore supply an integer. The private helper adds `AY` to calendar year and clamps invalid dates; its present second-argument `number()` spec is the singular mismatch. Existing focused tests exercise direct timestamp/datetime year addition, duration year application, negative sign semantics, and leap-day clamping. The authorised correction is solely `number()` to `integer()` on that private spec. Public exports/types/specs, neighboring private specs, every function body, tests, build configuration and CI policy remain outside the refinement.

A local OTP28/rebar3.27 run can exercise the affected native gates but cannot reproduce the actual OTP29/rebar3.28 Dialyzer implementation or its full hosted test/coverage environment. The spec-only edit therefore retains the147 runtime witnesses by demonstrating equality of all non-spec abstract forms, while requiring fresh local native gates and a new actual ten-job hosted run on the eventual final PR head. A green local run cannot close S03-05; the recorded nine-success/one-failure run remains failure evidence until the parent-owned rerun is reviewed.

## Refinement01 candidate and validation

### Exact source change and preservation checks

- On baseline `fcc1fe22e1651e133e20b7eb798dece82969e348`, the only source delta is `src/iso8601.erl:529`: `apply_years_offset(datetime(), number())` → `apply_years_offset(datetime(), integer())`. No function clause/body, adjacent private spec, exported function/type/spec, EUnit/PropEr file, rebar config, manifest, CI policy or README changed. The entire diff is one insertion/one deletion; `git diff --check` exits0.
- One-off AST check used OTP28.1.1 `epp:parse_file/3` on a temporary baseline copy produced by `git show fcc1fe22e1651e133e20b7eb798dece82969e348:src/iso8601.erl` and current `src/iso8601.erl`, then `erl_parse:map_anno/2` mapped annotations to0 and file attributes were excluded because paths differ. After removing the single `apply_years_offset/2` spec attribute from both parsed form lists, all remaining forms compare exactly. Separate projection asserted the corrected candidate spec has integer argument and compared exported functions, exported types, all named type/opaque attributes and public exported-function specs; these sets compare exactly. Result: `PASS parsed forms identical after removing only apply_years_offset/2 spec; public exports/types/specs identical` (15 public functions; 11 exported types). This is parsed-form equality after one declared spec removal, not a byte hash of runtime code.
- The first comparator attempt exited127: it incorrectly asserted the corrected private spec would be absent entirely. The observed exception was a test-script assertion (`no match of right hand side value true`), not an Erlang source or runtime failure. The comparator was corrected to assert the integer spec is present and to remove only that spec from each form list; the corrected comparison passed as above. Both attempt and correction are recorded rather than conflated with gate results.
- Current SHA-256 at precommit candidate: `src/iso8601.erl` `9fe7f27ada79809826c4f59a7c48f41b14cf67b5561b186b77d90a1b9dc5cdab`; `src/iso8601.app.src` `3c1d9d00c0d444c11e1043969b8b59305aeee640b167028ea0698876d55dacf1`; `test/iso8601_tests.erl` `ae801a4fd3c32aa09a9041aa0b989f4c34d711589c04a3c6bed6c541dc6d02c8`; `test/prop_iso8601.erl` `7a0d39b64438241599ae6017508d5b415a75cf82cbcd712da025db3816e2e17a`; `rebar.config` `39e573f074525a6d79381cb589f09bf42fa55398121a6951b6182274a4c6e513`. These four unaffected build/test paths retain their exact prior values.

### Fresh local gate receipts

Host tool identity: `rebar3 --version` exit0 → `rebar 3.27.0 on Erlang/OTP 28 Erts 16.1.1`. Gates ran sequentially in the owned source checkout; no global OTP29 was installed. Exact command output and exit markers are preserved in [`artifacts/refinement01-native.log`](refinement01-native.log). Results:

| Exact command | Exit | Observed result |
|---|---:|---|
| `rebar3 compile` | 0 | `iso8601` compiles. |
| `rebar3 xref` | 0 | Cross-reference analysis completes. |
| `rebar3 dialyzer` | 0 | One project file analysed with local OTP28 PLT; no warning. |
| `rebar3 eunit -c -v` | 0 | All238 tests passed, including existing year/timestamp/duration/leap-day cases. |
| `rebar3 proper -c` | 0 | All4 existing properties passed,100 tests each. |
| `rebar3 as test cover -v --min_coverage=100` | 0 | Module and total coverage100%. |
| `rebar3 as dev fmt --check` | 0 | All matched files use erlfmt style. |
| `rebar3 as dev lint` | 0 | Lint task completed. |

EUnit/PropEr/dev plugin downloads logged offline/download errors and used existing local cached Hex tarballs, then compiled and completed with the exits above. This is recorded input provenance; no claim of fresh network resolution. The exact hosted OTP29 Dialyzer red→green comparison cannot be made locally because this machine has OTP28/rebar3.27 only.

### Refinement boundary and remaining acceptance

The private type now matches both existing call paths and the public integer contract without narrowing any public type or changing execution. Reproducing local compile/xref/Dialyzer/EUnit/PropEr/coverage/fmt/lint and parsed-form equality supports retaining the earlier147 runtime observations unchanged; no hang probes or147 replay were rerun. The check does not establish OTP29 compatibility or acceptance. Parent/CDC still require actual final-head hosted execution of all10 jobs; final S03-05 remains open until all10 expected jobs and native steps pass, and S03-06 remains open for independent CDC disposition, retrospective, parent composition and Operator's distinct merge checkpoint. This is CC-proposed-done only for the bounded local refinement.

Source candidate commit: `845b1830b5acbfaed0909430e1ff011617a06817`, tree `084a53ea1aa64cb578dbccd5a609abdaf23ac220`, branch `codex/project02-otp-28-29`. `git show --check HEAD` exits0; commit names only `src/iso8601.erl` and carries the contiguous required coauthor trailers. Source worktree is clean after commit, one commit ahead of its original upstream tracking ref. No push was performed.

## Refinement02 intake, boundary repair, and validation — 2026-10-31

### Identity and preserved failure evidence

- Planning authority: open-set commit `8db980dd58c04bf59096cf4bdb6ba33c0f7ec93c`; assigned follow-up `cc-prompt-iteration02.md`. Initial and refinement01 prompts remain immutable. Current source baseline `845b1830b5acbfaed0909430e1ff011617a06817`, tree `084a53ea1aa64cb578dbccd5a609abdaf23ac220`, clean before this refinement on `codex/project02-otp-28-29`.
- Read this full refinement prompt, slice plan/ledger/CDC verification, both retained prompts, complete actual `src/iso8601.erl` (824 lines), full PropEr module (111 lines), affected parser/date/offset/interval test sections, and full second hosted log (598 lines). The 10-job JSON projection was parsed completely: run41262288775 on `845b183`;20–28 success, OTP29 compile/xref/Dialyzer success and EUnit failure, then PropEr/coverage skipped; the other OTP20 tier skips its three test steps. OTP29 EUnit reported236 passed and two failures. The failures are parse `2017-02-30` returning `2017-03-02`, and expanded negative week `-0001-W01-1` returning year -1 Jan4. Both original log and job JSON remain unchanged. Warnings compiling `rebar3_proper` are upstream plugin deprecation warnings, not project failures.
- Required Erlang guides 03-error-handling (440/440), 05-functions-and-pattern-matching (443/443), and 15-testing (355/355) were read in full; previous intake guides 11,17,02,04 remain pinned above. Error-specific contract and clause-order guidance support a single raw-input date check and a later negative-week guard; exact public rejection assertions preserve valid controls.
- Actual OTP28.5.0.8 and OTP29.1.2 calendar source copies supplied for this diagnosis are under `/private/tmp/iso8601-otp77-evidence/`; SHA-256 values recorded in the preceding CDC intake. Direct reads covered `valid_date/1,3`, `date_to_gregorian_days/3` and `day_of_the_week/1` plus dependencies. OTP28's Gregorian conversion rejects an impossible positive day by incidental `if_clause`; OTP29's arithmetic accepts it and its `valid_date/1` supports signed years. Negative week conversion similarly falls through with OTP29's expanded calendar arithmetic. OTP29 official calendar documentation was opened on 2026-10-31 at https://www.erlang.org/doc/apps/stdlib/calendar.html: `date()` requires valid dates; callers with unknown input are directed to `valid_date/1`; Gregorian calendar dates extend through year zero into negative years. The source contract does not validate by normalizing an invalid date.
- Source readback: `datetime/1` obtains `{Date, WeekOffsetH}` from `make_date/1`, then fractional-time and offset normalization run. The `make_date(Year,Month,undefined,Plist)` clause builds the raw date. A positive-year-only `calendar:valid_date(Date)` boundary check here therefore rejects invalid input before timezone normalization while preserving the intentionally open `Year =< 0` plain-date behavior. The next clause rejects negative week dates before week arithmetic; placing it after the calendar-date clause preserves negative plain dates and year-zero weeks. Existing nonzero-offset negative-year rejection remains. Incidental OTP28 reasons for the two now-stable rejected forms were reproduced before editing: `error:if_clause` for positive Feb30 and `error:function_clause` for negative week; both now deliberately raise `badarg`, as endorsed by parent/CDC. This is an explicit error-reason clarification. DATE001 week-range normalization and DATE002 nonpositive plain-date validity remain open.
- Read setup-beam's required README/version-selection/output sections and `action.yml` inputs/outputs, plus rebar3's documented release table and 3.28.0 release notes directly during intake; the section extents and conclusions are recorded in the original intake above. This follow-up does not change that workflow or claim local OTP28 as an OTP29 substitute.

### Source/test delta and native results

Only authorized source paths changed: `src/iso8601.erl` adds the positive raw-date validation and negative-week rejection clauses; the private integer spec from refinement01 is retained. `test/iso8601_tests.erl` makes both existing rejection tests exact `badarg` assertions, updates the incidental-crash comment, and adds public parse/parse_exact/expanded/interval regression and compatibility controls for invalid dates before offset normalization, positive leap day/timezone/week paths, year-zero week, negative plain date/offset policy, and expanded positive year. Public exports, types, public specs, return shapes and parser behavior outside those boundaries were not changed. `git diff` of production code is confined to `make_date/4`; neither build configuration nor README changed. The first EUnit attempt after adding tests had one assertion-shape mismatch: expanded year-zero parsing returns seconds `0.0`, not integer `0`. This was corrected to the existing exact expanded-time shape before the recorded final gate run; it was not a source or runtime failure.

Local host is `rebar 3.27.0 on Erlang/OTP 28 Erts 16.1.1`. All eight fresh gates ran sequentially; exact output and per-command exit markers are in [`refinement02-native.log`](refinement02-native.log): `rebar3 compile`0, `rebar3 xref`0, `rebar3 dialyzer`0, `rebar3 eunit -c -v`0 (249 tests), `rebar3 proper -c`0 (4 properties ×100), `rebar3 as test cover -v --min_coverage=100`0 (module and total100%), `rebar3 as dev fmt --check`0, `rebar3 as dev lint`0. No OTP29 was installed. These local gates do not close the required hosted criterion.

### Full retained-input replay on final candidate

The candidate production module was freshly compiled from the working source into isolated `/tmp/iso8601-refinement02-final-ebin`. The two immutable date/fractional escripts were copied to `/tmp` and only their `BeamDir` constants were directed at that isolated ebin. Both calendar probe modules were compiled against the same ebin. All four bounded replay commands exited0 and their full output is preserved in `refinement02-replay-date-31.log`, `refinement02-replay-fractional-10.log`, `refinement02-replay-calendar-42.log`, and `refinement02-replay-calendar-refined-64.log`: 31+10+42+64=147 observations. Worker probes retain their existing bounded timeout/kill/DOWN cleanup. Original audit scripts/modules and Project03 receipts were not modified.

Compared against Project03's accepted local28 receipt set (`../../../project03-runtime-hardening/slice01-interval-inheritance/artifacts/replay-date-31.log`, `replay-fractional-10.log`, `replay-calendar-38.log`, and `replay-calendar-refined-64.log`), the only functional output change is the deliberate invalid positive calendar input reason `if_clause`→`badarg` in the 31-date case. The fractional and original 42-calendar values match. Refined64 retains the same functional values/exceptions; stack traces naturally show this worktree's path and source lines shifted by the two new clauses (for example `apply_offset/4` 499→502), and a direct `erl -eval` launcher has `erl_eval` frames where the prior escript launcher had `init` frames. No other value/error result changed. This is applicability evidence on OTP28, not identical OTP29 calendar behavior and not a 147-gate acceptance substitute.

### Scope and handoff

The narrow runtime change addresses only the two reproduced OTP29 EUnit regressions. Earlier source histories and both failed hosted runs stay intact. DATE001, DATE002 and the remaining audit findings remain open; no broad repair, matrix/config change, push or PR occurred. The required 147 bounded probes did run; no unbounded hang call or hang-repair experiment was added. All six ledger rows were walked: CC proposes S03-01, S03-02, S03-03 and S03-04 done/reproduced with this evidence; S03-05 remains open for the parent-owned final-head actual10 hosted jobs; S03-06 remains open for independent CDC review, retrospective, parent composition and Operator checkpoint. CC does not self-accept. The expected all10 final hosted run must include OTP29 EUnit, PropEr and coverage after this exact source candidate is reviewed and published. Existing Project03 links in this section are relative from `artifacts/` with three parent traversals (`../../../project03-runtime-hardening/...`).


The normalized OTP28 EPP projection against source baseline `845b183` compared all `export`, `export_type`, `type`, `opaque`, and `spec` attributes after annotation normalization; result `PASS public export/type/spec projection identical`. Two malformed comparator invocations exited nonzero before this corrected one-off projection passed; they were harness invocation mistakes, not source failures. The initial post-edit EUnit attempt likewise exposed only the incorrect integer-seconds expectation for expanded parsing; final corrected native receipts above are authoritative.

Source refinement02 commit `43a37409bbca41ea835faffb7a77cb7087aa1b8c`, tree `3224b1e0da9285ab67349aadc3b59ffa11bfe8ab`. The immediately prior unpublished commit pin was `3071050f501a9651356061d5c9da859cb9da862b` with the identical tree; only its message was amended to make the required co-author trailers contiguous. The source commit contains only `src/iso8601.erl` and `test/iso8601_tests.erl`, has adjacent required Codex/Billo co-author trailers, and `git show --check HEAD` exits0. Source worktree is clean after commit; no push or PR action occurred.


### Commit metadata correction — no source/tree change

The original refinement02 source commit `3071050f501a9651356061d5c9da859cb9da862b` was amended to `43a37409bbca41ea835faffb7a77cb7087aa1b8c` solely because its commit message had a blank line between the two required co-author trailers. Both source commits have tree `3224b1e0da9285ab67349aadc3b59ffa11bfe8ab`; their source contents are identical. The replacement message was read back literally and its trailer block has adjacent lines.

The two unpublished refinement02 planning commits were likewise reworded only to join their trailer lines: `2e49d5f662113675e7a34e5b9ca7be0f0ddae7c1` → `ee1d7ff25d4b10f9c2c5b0ad47ca48afea94488d` (tree unchanged at `0d22a8663a9b1839b8e117cff86593f8c6397b49`), and `81072a04abfbf47883f2d0544b3bc1f48e46c742` → `5519228413aa597ddb099ea5f3dc5b1a8d656596` (tree unchanged at `145a70cfb9bab763f233355ae58b552c368dcc50`). Both replacement message bodies were read back and their trailer lines are adjacent. The following planning commit records these metadata-only pin corrections and the final source pin; it introduces no runtime iteration or validation rerun.
