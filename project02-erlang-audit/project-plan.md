# Project02 — Erlang audit

- project: `project02-erlang-audit`
- status: Arc01 reviewed; OTP28/29 CI and the first two Project03 repair checkpoints merged; main e332032 passes all ten jobs; Arc02 audit and three runtime parser repairs remain open
- depends-on: historical preservation completed at planning `213bfc7`; source baseline `bcfd94a`
- blocks: any claim of a complete current audit; does not block the separately authorized Project03 repairs of accepted Arc01 findings
- related: `project01-v1.4-maintenance`; `project03-runtime-hardening`; iso8601 v1.4.0
- version: 1.9 — 2026-11-01 UTC

## Outcome and boundaries

Deliver an evidence-based diagnosis of all first-party Erlang and its public, test, build and compatibility contracts. Findings have stable IDs, severity, precise source locations, expected/observed public behavior, concrete fix directions and independent reproduction. Final project delivery includes an Erlang report, complete audit index, historical-claim reconciliation, and evidence-backed modernization synthesis. Arc01/02 remain diagnosis-only. A narrow later implementation exception for OTP28/29 CI additions and compatibility fixes necessary for them was directly requested by the Operator on2026-10-31; Arc03 owns it. That diagnosis-only boundary remains in this project; the later Operator decision recorded below authorizes runtime remedies separately in Project03. This project does not certify a release.

The Operator authorized historical migration first, the standard orphan planning layout, then a new indexed project and Autonomous Mode for **ONE ARC ONLY**. Arc01 stops review-ready for human review and merge discussion. Arc02/03 cannot start before the Arc01 checkpoint and subsequent execution decision. A diagnosis-only arc may have no source diff; its useful delivery is committed planning evidence. Source fixes outside the narrow later OTP28/29 compatibility exception belong to the separately authorized Project03 hardening plan. The original Arc01 stop was reached and its review was subsequently dispositioned below; it did not automatically authorize Arc02/03 execution. The initial diagnosis-only authority excluded publication. The later dated CI-priority decision below authorizes the necessary source branch push/PR and automatic hosted checks for that CI increment; its merge/release and other external actions still require their own Operator decision.

## Audit architecture and complete map

Source baseline is `bcfd94a`, the guidance cleanup above `c594869` (v1.4.0). The library is one pure synchronous module; callers own processes and failure handling. No services, supervision tree, workspace members, FFI, network or persistence subsystem exists. README/API/EDoc are current context; no standalone current architecture/design document is tracked. Historical interval design is provenance, not a current assignment. Record this discovery gap in audit summaries and assess its consequence proportionately.

| Tracked surface | Role and review destination |
|---|---|
| `src/iso8601.erl` | All 744 lines; 15 public functions and 11 intentionally exported types; Arc01 runtime and Arc02 language/tooling reconciliation |
| `src/iso8601.app.src` | Library app/version/dependencies/package metadata; Arc02, context for Arc01 |
| `test/iso8601_tests.erl` | EUnit including parser, arithmetic, interval, expanded-year cases and capture helper; runtime witnesses Arc01, full test quality Arc02 |
| `test/prop_iso8601.erl` | Four properties and duration helper; runtime witnesses Arc01, oracle/population quality Arc02 |
| `scripts/check_coverage.escript` | First-party Erlang executable; all 91 lines and exit/coverage contracts Arc02 |
| `README.md` | Public usage, supported representations/limits, compatibility badge; both arcs |
| `rebar.config`, `elvis.config`, `Makefile`, `.github/workflows/ci.yml` | Tool/profile/gate/platform contracts; context Arc01, diagnosis Arc02 |
| `AGENTS.md`, `CLAUDE.md` | Governing instructions and symlink; both arcs |
| `.gitignore`, `LICENSE`, `priv/images/logo.png`, `priv/images/logo-large.png` | Repository/package boundaries; no executable language audit of license/images |

There are 16 tracked entries including the symlink. Erlang is audited including its escript and manifest. Make and YAML are detected contract/configuration languages with no matching installed language skill; inspect their operational promises without claiming a separate language audit. Markdown is contract/provenance text, PNGs are static assets. Generated `_build`, `.eunit`, beams, docs/logs, dependency caches, `.git`, nested worktrees and ignored workbench scratch are excluded from source populations. Archived history remains preserved and its acceptance rows stay open.

## Roadmap

1. **Arc01 — runtime contracts:** independently checked diagnosis of date parsing/formatting/expanded representations and arithmetic/duration/interval composition. First usable increment: public-API date-contract findings and reproducible probes. Delivery is the owning slices' artifacts and an arc close packet on `planning`.
2. **Arc02 — validation and audit synthesis:** full test/escript/tooling/OTP/package-contract and public-spec reconciliation audit; reconcile historical findings against actual source/authoritative OTP documentation; assemble final one-language report, index and modernization synthesis with compatibility classifications. Consumes accepted Arc01 findings at their pins; roadmap only, no detailed arc/slice directories yet.
3. **Arc03 — OTP28/29 CI compatibility:** add OTP28 and29 to the CI support contract and make only compatibility fixes necessary for those additions. Preserve existing OTP20–27 support and older-floor discipline unless a reproduced blocker requires an Operator decision; do not drop versions by convenience. Validate actual selected toolchain pins/matrix and record failures. If necessary compatibility fixes change the source pin, reproduce affected audit contracts/findings and reconcile final report applicability; preserve original bcfd94a/date receipts without presenting them as acceptance of changed source. Roadmap only; no detailed arc/slice directories, CI/source edits or execution before current Arc01 checkpoint.

## Dated scope amendment — 2026-10-31

Direct Operator steering: “we're going to need to bump CI to support OTP28 and29 (we might need to provide fixes for those additions, too ...)”. This authorizes planning the narrow Arc03 implementation exception; the current one-arc execution endpoint remains intact. Current `.github/workflows/ci.yml` still configures20–27; neither28 nor29 has been added or validated as a project matrix job. Runtime findings unrelated to the additions remain diagnosis-only.

Parent read primary documentation on2026-10-31: official [OTP29 downloads](https://www.erlang.org/downloads/29) points to OTP-29.1.2 and official `erlang:29` Docker; [OTP28 downloads](https://www.erlang.org/downloads/28) points to OTP-28.5.0.8 and `erlang:28`. [setup-beam README](https://github.com/erlef/setup-beam/blob/main/README.md) lists Ubuntu22.04 with OTP24.2–29. [rebar3 compatibility table](https://github.com/erlang/rebar3) lists3.28.0 forOTP27–29 and3.25.1 forOTP26–28. These support candidate future substrates, not tested installs/pins/matrix passes. In particular, review the current >=26→3.25 selection before adding29. No binary was installed by this documentation check.

## Selected next increment: OTP28/29 CI — 2026-10-31

The Operator explicitly prioritized OTP28/29 expansion as the next work so future changes run on those releases, then confirmed PR#76 was already merged. GitHub main is now1d85d0a98a21750f83355a855dcc8fb4728758b4, with the exact reviewed first-repair source tree90b1466fabc482ef45debc846bcd0657db625153. This is the CI implementation baseline; original diagnosisbcfd94a evidence remains historical and is not overwritten.

Execution order is now completed Arc01 diagnosis → **roadmap Arc03 CI compatibility next** → later audit/runtime work by their recorded gates. Stable numbering does not define dependency order. Preserve OTP20–27 and its OTP20 test exclusions; add28/29 with source-grounded toolchain pins, unchanged coverage floors and only fixes proved necessary for those additions. The implementation fits one coherent checkpoint; detail lives directly in `slice03-otp-compat/` under canonical one-slice collapse rather than a ceremonial arc wrapper. A dedicated source worktree/branch `codex/project02-otp-28-29` starts at main1d85d0a.

Continue existing Autonomous Mode with CDC GPT-6.1 Sol High and fresh CC GPT-6 Luna High. CDC prepares/independently reviews; parent retains integrated accountability and publishes a separately reviewed CI PR to obtain the required actual ten-job OTP20–29 hosted run. The selected public PR workflow and CI request authorize that scoped source push/PR and its automatic validation; the orphan planning branch stays local. Stop at this single CI checkpoint for Operator review/merge. Do not resume Project03's seven runtime repairs, broaden the remaining diagnosis, merge the new CI PR or release automatically. Any necessary compatibility source fix must preserve API and reconcile affected accepted contracts at the changed pin.

This direct priority decision supersedes the earlier roadmap-only/no-Arc03-execution statements; it does not mark P02-06 done. Project02 Arc02 and project acceptance remain open. Primary toolchain compatibility must be freshly verified and all ten hosted job outcomes inspected; documentation or local OTP28 alone cannot satisfy the CI criterion.

## Workflow, models and ownership

Operator-selected Autonomous Mode uses two actual contributor contexts: CDC OpenAI GPT-6.1 Sol, High (planning, source-grounded review and independent reproduction); CC OpenAI GPT-6 Luna, High (bounded diagnosis/evidence assembly). These are explicit Operator choices relayed in the orchestration assignment; no fallback is authorized. Parent orchestration context retains integrated accountability and independently reviews arc composition. Model names alone do not establish independence.

Source audit checkout: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit`, branch `codex/project02-arc01-audit`, baseline `bcfd94a`. Planning: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/planning`, orphan branch `planning`. CDC owns planning except explicit sequential CC handoffs. CC owns generated outputs in the isolated source checkout while executing; no tracked source mutation. No overlapping installs/builds/scanners. Each slice uses reviewer-authored `cdc-verification.md` plus CDC `cdc-retrospective.md`; arc closure assembled by CDC remains subject to parent independent composition review and Operator gate.

## Evidence and validation contract

Audit date captured with `date +%Y.%m.%d`: `2026.10.31`. Per-slice durable artifacts override code-auditing's `workbench/` default under the repository/Operator standard. Keep distinct failed, retried, CC and CDC observations in existing run logs; no stale May build output counts as a current baseline. Exact commands, exit statuses, actual selected populations and limits are required. Source expected values come from documented contracts or independently derived calendar cases, not another call to the same implementation. Hang hypotheses use monitored workers with a timeout and kill/cleanup.

Actual preflight observed local rebar3 3.27.0 / OTP 28, outside configured OTP20–27. Revalidate in execution. CI runs compile/xref/Dialyzer on20–27 and tests/PropEr/coverage on21–27; R15B03 is intended historical floor, not demonstrated here. Rebar check/CI requires100% coverage; Make script requires95%; neither is lowered. Attempt applicable native gates in a fresh isolated checkout; preserve failures. If dependency loading is unavailable, disclose blocked gates and use fresh local erlc/erl public-API probes as bounded evidence, never a replacement claim of full CI success.

## Historical migration independent check

CDC independently compared all12 archived files against source bytes, SHA256 and source/archive nanosecond mtimes; all match. All30 historical acceptance rows across9 ledgers remain OPEN; exactly2 arcs/6 slices. All81 authored relative links resolve; all14 task/mainline patch-ID pairs match; planning has one parentless root. History remains historical and is not accepted retroactively. Migration baseline `213bfc7`; these checks were read-only and no historical file was changed.

## Known uncleared watch items

Arc02 owns public-type/tooling reconciliation, including `parse_exact/1` src129 returning`calendar:datetime()` in its spec while its documented/exercised result has fractional second6.5 for`20120203T040506.50`. Private`datetime()` src40 permitsfloat; calendar second() is integer0..59. This source-pinned mismatch was observed during Slice01 review and remains uncleared for Arc02 severity/fix-compatibility assessment; it is not silently accepted or repaired in Arc01.

Slice02 explicitly leaves the reduced-precision zoned-start contract question uncleared for Arc02 public-contract/test reconciliation: `parse_interval("2007-12-14T13:30+02:00/15:30:00")` raisesbadarg before inheritance, while fullseconds succeeds (source299–387). Current README/tests do not separately promise this form; observed rejection is not a confirmed defect or standards conclusion. Arc02 must disposition that evidence rather than silently clear it. Zero-duration bareP serialization, by contrast, was reviewed against historical omission/always-P design and is not counted as a defect.

Arc01 now has9 stable findings (3High/5Medium/1Low) at unchangedbcfd94a; both slices independently accepted, exact15-export/74-form helper map assembled. This is runtime-only delivery. The earlier v1.3 statement that parent composition and Operator decision were pending is superseded: parent composition was independently reproduced at `cfbf539`; the Operator reviewed the findings and chose remediation first as recorded below. The v1.2 CI exception superseded the original all-project repair prohibition only for later OTP28/29 compatibility, without extending current execution.

## Operator review and hardening handoff — 2026-10-31

After the independent parent composition at `cfbf539`, the Operator stated: “I've looked at the findings — I want us to fix them all :-D” and asked how to start. This dispositions Arc01's human checkpoint: the runtime diagnosis is reviewed, and all nine accepted findings are authorized for repair. There is no audit source diff to merge; this decision does not perform or authorize a source merge, push or release.

[Project03 — runtime hardening](../project03-runtime-hardening/project-plan.md) consumes the two accepted Arc01 reports at source `bcfd94a`. It starts with the two High interval corruption findings, followed by arithmetic/precision and parser contracts, using the existing Sol High CDC / Luna High CC arrangement and stopping after each repair arc for human review. This is a bounded handoff from accepted runtime findings, not a claim that the entire diagnostic audit is complete. Arc02's remaining test/tooling/public-spec/historical-claim audit and Arc03's requested OTP28/29 CI work stay open here; neither has begun. Any changed-source audit applicability must distinguish the original `bcfd94a` observations from later regression evidence.

## Replay denominator correction — 2026-10-31

During Project03 remediation, CC and CDC found that the original calendar runner was undercounted in earlier audit summaries. Parent independently inspected the preserved runner and transcript: `calendar_interval_probes.erl` executes **38 ordinary observations plus four bounded calls, 42 total**, and `calendar-interval-probes.out` contains 42 ` => ` records. The refined runner/transcript has **58 ordinary plus two stack observations plus four bounded calls, 64 total**. Earlier “38 initial observations” wording excludes the bounded calls and is superseded wherever it described the total. The other replay populations remain 31 date observations and 10 fractional comparisons, giving 147 observations across four runners.

The original scripts, transcripts, reports and issued prompts remain unchanged. Parent's previous byte-identical replay at `cfbf539` executed each complete runner, so it included all 42 initial calendar observations; this correction changes count metadata, not the findings, timeout cleanup, source population or prior execution coverage. Current Project03 evidence must use the corrected denominator, distinguish its changed source pin, and preserve prior failed and accepted observations.

## Definition of done

See sibling `ledger.md`. Project completion requires all three arcs, complete mapped scope, independent evidence and explicit Operator acceptance. Arc01 findings are not a claim that the whole project or prior release is accepted.

## Version history

- v1.0 — 2026-10-31: CDC opened two-arc diagnosis roadmap after byte-preserved historical migration; only Arc01 execution authorized.

- v1.1 — 2026-10-31: Arc01/Slice01 review explicitly identified the known parse_exact return-spec obligation for already-roadmapped Arc02; no new arc or source scope added.

- v1.2 — 2026-10-31: direct Operator request added roadmap Arc03 for OTP28/29 CI and only necessary compatibility fixes, preserving existing support and the current Arc01 stop gate. Parent primary-document provenance retained above; no current configuration change.

- v1.3 — 2026-10-31: Arc01/Slice02 close surfaced the explicit reduced-precision-zone contract obligation, retained zero-duration intent disposition and runtime delivery status. No new execution or repair scope; parent/human gates stay open.

- v1.4 — 2026-10-31: Operator reviewed Arc01 findings after parent composition and explicitly authorized all nine repairs; handoff to separate Project03 supersedes the former pending-review/repair-authorization status. Remaining diagnostic and OTP28/29 CI obligations remain open, with no automatic next-audit-arc execution.

- v1.5 — 2026-10-31: Project03 replay surfaced an inherited denominator error; parent confirmed 42 initial calendar observations (38 ordinary +4 bounded), retained 64 refined observations, and documented full-run applicability without altering original reports/probes.

- v1.6 — 2026-10-31: direct Operator priority selected OTP28/29 CI as next increment before future source changes; PR#76 merge supplies exact main1d85d0a baseline. CI roadmap Arc03 is advanced ahead of later audit/runtime work, with scoped public PR/automatic ten-job validation and one-checkpoint merge stop; older roadmap-only execution status is superseded.

## CI publication checkpoint — 2026-11-01 UTC

After CDC independent prereview at planning `f5b0579`, parent published exact source `fcc1fe22e1651e133e20b7eb798dece82969e348` and created [PR#77](https://github.com/erlsci/iso8601/pull/77), targeting main `1d85d0a`. The two-file candidate adds28/29 with rebar3 3.28.0 and updates AGENTS ranges; production/tests/build inputs remain unchanged. [Run41261632042](https://github.com/erlsci/iso8601/actions/runs/41261632042) has scheduled all ten intended jobs. This is publication and scheduling evidence, not a passing matrix or integration claim. S03-05/06, parent composition and Operator merge checkpoint remain pending; no later runtime or audit increment begins.

- v1.7 — 2026-10-31: scoped CI candidate independently prereviewed and published as PR#77; all ten hosted jobs scheduled on exact fcc1fe2, with acceptance and merge gates retained. Hosted events use their actual UTC date,2026-11-01.

## First hosted compatibility blocker — 2026-11-01 UTC

PR#77 run41261632042 completed failure at exact fcc1fe2: OTP20–28 all nine jobs passed; OTP29.1.2 / ERTS17.1.1 / rebar3 3.28.0 passed compile/xref but failed Dialyzer with one underspec warning at src529, private `apply_years_offset(datetime(), number()) -> datetime()` versus inferred integer year offset. OTP29 EUnit/PropEr/coverage were consequently skipped and are not accepted. This reproduced blocker activates the existing conditional compatibility-fix scope. CDC owns a preserved, bounded refinement before CC mutation; parent holds publication until independent prereview and will require a full ten-job run on the revised head. The original failed receipt is retained, and no unrelated runtime fix or gate weakening is authorized.

## Second hosted compatibility blocker — 2026-11-01 UTC

Reviewed refinement01 `845b1830b5acbfaed0909430e1ff011617a06817` corrects only the private year-offset spec. Both CC and CDC freshly passed all eight local native gates; parent independently confirmed exact one-line production delta and unchanged runtime/public declarations/test/build bytes. It was published to PR#77 after prereview `f58ef7a`. The installed CLI PR-body edit failed on a deprecated Projects-classic query; REST PATCH applied the same prepared body successfully. This was a tooling retry, not a product failure.

[Run41262288775](https://github.com/erlsci/iso8601/actions/runs/41262288775) again completed9success/1failure on exact845b183. OTP29 compile/xref/Dialyzer all pass, establishing the first fix. Its EUnit reports236passed/2failed: `parse("2017-02-30")` returns March2 and `parse_expanded("-0001-W01-1")` succeeds, contrary to existing rejection tests. OTP29 PropEr/coverage remain skipped after that failure. The two PropEr dependency compiler warnings are distinct from these actual test failures. Existing20–28 jobs still pass.

The next bounded compatibility refinement must preserve the two established rejection contracts explicitly across releases, with real positive/negative controls, affected runtime-audit applicability and unchanged public API/older support. It must not skip/version-condition tests or opportunistically fix all seven remaining runtime findings. Any actual overlap with a finding must be identified honestly. Source mutation awaits a preserved CDC refinement; publication awaits its independent prereview and another full exact-head ten-job run.

## Final CI technical checkpoint — 2026-11-01 UTC

After independent prereview `d4c6a91`, parent published source `43a37409bbca41ea835faffb7a77cb7087aa1b8c`, tree `3224b1e0da9285ab67349aadc3b59ffa11bfe8ab`, to [PR#77](https://github.com/erlsci/iso8601/pull/77). The four-path diff contains the CI/guidance expansion, one private integer-spec correction, two parser-boundary rejection checks and their regressions/valid controls. Positive raw calendar dates are validated before offsets; negative week dates reject explicitly. Valid expanded negative calendar dates, year-zero week behavior, public declarations, old toolchain selections and100%coverage floor remain preserved. Invalid forms now raise deliberate `badarg`, replacing incidental older-calendar reasons; no other runtime finding is claimed repaired.

[Run41263433752](https://github.com/erlsci/iso8601/actions/runs/41263433752) passed all ten jobs on exact43a3740, last job completed2026-11-01T02:02:32Z. Parent independently inspected complete job JSON and logs: exactly20–29; compile/xref/Dialyzer all successful; only20 skips its existing three test gates. Each21–29 executes249 EUnit,4×100 PropEr and100%module/total coverage. Actual new substrates: OTP28.5.0.8/ERTS16.4.0.7 and29.1.2/ERTS17.1.1, both rebar3.28.0. The earlier two9/10 failed runs remain preserved rather than replaced by the final pass.

CC and CDC independently ran all eight native gates and all147 bounded local28 audit observations on the runtime change. The observations retain the seven open findings; the deliberate positive-invalid-date reason change and stack/launcher metadata are disclosed. These are local28 applicability results, not a claim every prior observation has identical behavior on29. Parent source composition, complete scope/commit review and actual ten-job inspection are recorded in the slice closing report. P02-06 is now done/reproduced after the separately attributable human merge and exact-tree integration checks below. The complete audit and all seven remaining repairs stay open. No next audit/repair arc or release starts automatically.

### Remaining tooling watch

OTP29 emits upstream compiler warnings in PropEr1.5.0 (`proper_statem.erl` catch deprecation and `proper_erlang_abstract_code.erl` is_record/1 auto-import clash) and rebar3_proper (`rebar3_proper_prv.erl` catch deprecation). Final jobs pass, but this is not a warning-free toolchain claim. No suppression or dependency upgrade was made. These specific upstream sites carry into the already-open Arc02 tooling/dependency review; first-party compilation and actual test outcomes are distinct evidence.

- v1.8 — 2026-10-31: final CI head passes all ten hosted jobs after two evidence-led refinements; records actual versions/populations, preserved failures, upstream warning carryover, user merge and independently verified main integration. Original audit/project acceptance and seven runtime repairs remain open.

## Operator merge and main integration — 2026-11-01 UTC

The Operator stated: “all passing! I've merged the PR :-D”. GitHub confirms PR#77 merged by `oubiwann` at2026-11-01T02:06:02Z into main `3a1cdfd17bd4dfaf36c9eaa34c5102944a3e0828`. Its tree `3224b1e0da9285ab67349aadc3b59ffa11bfe8ab` is exactly the reviewed43a3740 tree. Parent fast-forwarded clean local main and independently verified that tree. Rebased commits `dfb1924`, `69ddc49`, `3a1cdfd` retain both adjacent coauthor trailers.

Automatic [main push run41263775940](https://github.com/erlsci/iso8601/actions/runs/41263775940) also completed successfully on exact3a1cdfd, last job2026-11-01T02:07:26Z. Parent inspected all ten job records and native step outcomes, including only20's intentional test skips. The prior final-PR full-log/version/population evidence remains applicable to the identical tree; no extra native or full-log replay was needed for a rebase that preserved every file. This dispositions the CI increment's human merge/integration checkpoint and closes P02-06. It does not accept the remaining audit, seven runtime repairs or a release, and no later increment has started.

## Subsequent remediation applicability — 2026-11-01 UTC

Project03 [Slice02 arithmetic/precision](../project03-runtime-hardening/slice02-arithmetic-precision/closing-report.md) repairs CAL001/CAL002/DATE003/DATE005 after the completed CI checkpoint. Its reviewed source bd67bd0 and merged main e332032 have the same tree 72efe0dc; [PR#78](https://github.com/erlsci/iso8601/pull/78) and automatic merged-main CI both pass all ten OTP20–29 jobs. Fresh local OTP28 replay covers all 147 original observations with their actual current applicability; the original diagnostic reports and runners remain unchanged. Together with the earlier interval repair, six runtime findings are repaired and three parser findings remain open. This supersedes the preceding no-later-increment/seven-open runtime state, without accepting the remaining Project02 diagnostic work.

The six upstream OTP29 warning renderings/four dependency sites remain the existing Arc02 tooling follow-up, separate from the passing first-party checks. No further diagnostic or parser increment starts automatically. The Operator selected eventual 1.4.1 in a separate release-preparation PR after all repairs land; no version bump or release is included here.

- v1.9 — 2026-11-01 UTC: subsequent Project03 arithmetic/precision checkpoint merged and independently reconciled; preserves original audit provenance, remaining diagnostic obligations, three open parser findings and the upstream tooling watch.
