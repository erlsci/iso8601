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
