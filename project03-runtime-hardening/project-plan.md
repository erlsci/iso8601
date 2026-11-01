# Project03 — runtime hardening

- project: `project03-runtime-hardening`
- status: all nine finding criteria technically repaired; final parser checkpoint independently accepted after ten-job PR79CI and Operator merge10a182; parent composition/integration and whole-project gates remain separate
- depends-on: accepted Project02 Arc01 diagnosis at planningcfbf539; sourcebcfd94a v1.4.0
- related: `project02-erlang-audit`; historical `project01-v1.4-maintenance`
- blocks: claims that the nine accepted runtime findings are repaired
- version: 1.12 — 2026-11-01 UTC

## Outcome, authority and endpoint

Repair all nine accepted Project02 Arc01 findings while preserving the 15 public exports, 11 intentionally exported types, return shapes and broad OTP compatibility. Add meaningful regression tests that fail unchanged source and pass repaired source, source-grounded documentation, independently reproduced validation and a finding-by-finding applicability handoff. Original audit reports remain immutable dated evidence, not rewritten into green reports.

Direct Operator decision: “I've looked at the findings — I want us to fix them all :-D How do we get started?” Parent relayed continued Autonomous Mode with CDC GPT-6.1 Sol High and fresh CC GPT-6 Luna High, and a stop after ONE repair arc/increment for human review and merge discussion. The first coherent interval repair fits one slice; canonical one-slice collapse puts its documents directly in `slice01-interval-inheritance/`, with no ceremonial arc wrapper. This slice is the first repair-arc checkpoint for the authorization; the branch's arc01 spelling does not require extra arc documents. Parent independently gates its final composition and the Operator retains the review/merge/next-increment decision. Original v1.0 authority excluded push/PR/merge/release and later increments. The subsequent direct PR request below supersedes only the source-branch push/PR/CI-delivery exclusion; merge, release, planning-branch publication and later repair increments remain unapproved.

Project02 remains incomplete: its Arc02 tests/escript/tooling/type/historical/synthesis audit and Arc03 OTP28/29 CI plus necessary compatibility work stay open, separately owned. They are not cancelled or silently absorbed into runtime repair. Known parse_exact public return-spec and reduced-precision zoned-start questions remain Project02 obligations. This project does not claim a complete audit, supported OTP matrix or release certification.

## PR delivery authorization — 2026-10-31

After reviewing the first checkpoint, the Operator said: “Since this is a publicly consumed project, let's do a PR”. This authorizes pushing the reviewed source candidate `7e93c5d89cb8b14e7a2f5f7a0219201ee1bcfc38` on `codex/project03-arc01-intervals` to GitHub origin and opening a reviewable PR against `main`, including its automatic CI runs and monitoring their terminal result. GitHub main was refreshed and verified at `c594869`; there is no base drift. Consequently the PR also contains the previously authorized guidance cleanup `bcfd94a`, not yet published upstream. Source API/behavior/test evidence remains applicable at the exact reviewed tree. The orphan planning branch stays local. No merge, release, or next increment is inferred. Delivery completed: [PR #76](https://github.com/erlsci/iso8601/pull/76), against main, head7e93c5d. [Hosted CI run41260496064](https://github.com/erlsci/iso8601/actions/runs/41260496064) completed successfully in all8 OTP20–27 jobs; OTP20 test/coverage steps remain intentionally skipped, OTP21–27 execute them. Exact job URLs, steps, head identity and inspection commands are in the slice closing report. Source required no further correction. Merge/continuation is still pending.

## Operator merge and revised priority — 2026-10-31

The Operator merged PR#76 and explicitly selected OTP28/29 CI as the next work so future changes are exercised on those releases. GitHub confirms rebase-merged main1d85d0a with exactly the reviewed7e93c5d source tree; the first checkpoint is integrated and its human gate is dispositioned. [Project02](../project02-erlang-audit/project-plan.md) owns the next CI increment, retaining its narrow compatibility scope and the existing Sol High CDC / Luna High CC setup. This supersedes the earlier unmerged/continuation-pending status and moves CI ahead of this project's remaining arithmetic/precision and parser-contract increments. Those seven findings and project acceptance remain open. No next runtime increment or release is inferred.

## Full remediation roadmap

| Increment | Capability, findings and destination | Dependency / current state |
|---|---|---|
| First repair checkpoint — collapsed Slice01 | Correct interval signed positive-year classification/inheritance (ERL-CAL-003) and omitted-zone inheritance (ERL-CAL-004); source module, public regression tests and README | Integrated via PR#76 at main1d85d0a (tree equals reviewed7e93c5d); human merge/next-priority decision complete |
| Active second repair checkpoint — collapsed Slice02 | Bounded arithmetic floor behavior CAL001, float-anchor composition CAL002, formatter carry DATE003 and exact fractional-unit retention DATE005 | [Slice02](slice02-arithmetic-precision/slice-plan.md) at source3a1cdfd; bounded badarg below year0, fractional-anchor/exact-unit retention and formatter carry, preserving both actual parse/1 legacy rules. Four finding rows are done/reproduced at bd67bd0 after independent native8/147 and actual ten-job PR78 CI. Operator merged PR78 into maine332032; parent composition/integration record follows separately. Parser increment remains roadmap-only; no next work is inferred. |
| Active third repair checkpoint — collapsed Slice03 | Validate week boundaries DATE001 and non-positive-year dates DATE002; make declared iodata inputs work consistently DATE004 | [Slice03](slice03-parser-contracts/slice-plan.md) activates at integratede332032. Validate original calendar/week fields before offsets and normalize all FOUR declared iodata boundaries. Three finding rows remain open until independent technical acceptance; parent16/human17 and whole-project12/13 remain distinct. |

The [project ledger](ledger.md) tracks every stable ID individually. Future decomposition can refine dependencies from real source evidence, with versioned decisions; no future arc/slice directories are opened now. All current source surfaces are the existing one-module library; this increment changes only `src/iso8601.erl`, `test/iso8601_tests.erl` and `README.md`. Existing `test/prop_iso8601.erl`, app manifest, Make/rebar/CI/elvis and guidance are context, not repair targets. No new dependencies, syntax floor, record/API representation, exported helpers or version bump.

## Worktrees, workflow and evidence

Source `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project03-arc01-intervals`, branch `codex/project03-arc01-intervals`, baseline `bcfd94a08fa7a427eb1469679897e4d92d808edf`. Integration home is `main`; this increment delivers reviewable source commits, not an integrated/released result. Planning `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/planning`, orphan `planning`; only this new subtree belongs to this CDC/CC. Parent owns existing Project02/README updates. Explicit named-path commits keep shared-index ownership separate.

CDC owns project/slice plans, independent `cdc-verification.md` and `cdc-retrospective.md`; CC owns the allowed source paths and slice artifacts/ledger/closing-report during its handoff. Parent owns independent checkpoint composition after CDC closes the slice; Operator gate stays open. Fresh CC uses the explicitly selected Luna High setting; no fallback inferred. At most five preserved numbered refinements per slice. No overlapping rebar/plugin/cache mutation; native loaders run sequentially.

Durable supporting evidence goes to owning slice `artifacts/`; existing audit artifacts are read-only provenance/replay inputs. Retain exact failed/red/green/retried/escalated commands, exit statuses, test populations and attribution in one runlog. Original audit probes must be replayed against changed source and all nine findings dispositioned: current two repaired, remaining seven still evidence or explicit changed applicability, never all-green by assumption.

## Validation and definition of done

Native source gates: compile, xref, Dialyzer, EUnit, PropEr, configured100% test coverage, dev fmt check and lint; do not lower either rebar/CI100% or Make95% threshold. Local OTP28/ERTS16.1.1/rebar3.27 is observed, not a20–27/R15B03 matrix pass. New source checkout starts with no build output. Prior audit native passes are baseline context, not new-candidate validation. Attempt actual native gates afresh; preserve dev provider/network failures and try a targeted escalated native attempt before reporting blocked. Known cached source packages may be fresh build inputs if recorded; old beams cannot be baseline proof. Do not repair unrelated tooling configuration here.

Completion requires all nine finding rows independently verified, final applicable source/validation evidence, useful existing behavior retained, honest Project02 obligations and explicit Operator acceptance. The first checkpoint may close only its own two finding rows; later fixes and project acceptance stay open. See sibling ledger and active slice plan for exact acceptance mapping.

## Version history

- v1.0 — 2026-10-31: direct Operator all-findings repair decision opened three-increment roadmap; the first two interval findings form one collapsed slice and the authorized first checkpoint only.

- v1.1 — 2026-10-31: first interval slice independently accepted for CAL003/CAL004 at7e93c5d. [Verification](slice01-interval-inheritance/cdc-verification.md) and [retrospective](slice01-interval-inheritance/cdc-retrospective.md) carry current receipts, preserved147-observation audit applicability and producer-path/population/reading watch items. Only these two finding rows close; seven repairs, parent composition, human merge/continuation and all project/P02 obligations remain open.

- v1.2 — 2026-10-31: parent independently reviewed the accepted slice and composed the interval repair with19 exact controls, actual formatter composition, final238 EUnit and unchanged15/11 export/type sets. P03-10 closes; P03-11 and all seven later repairs/project gates remain open. Prior pending-parent status is superseded; no merge or next increment is inferred.

- v1.3 — 2026-10-31: direct Operator request authorized publishing the reviewed source branch as a GitHub PR against main and following hosted CI; refreshed basec594869 means previously authorized guidance cleanup is included. Parent composition is already accepted; merge/continuation and later repairs remain pending.

- v1.4 — 2026-10-31: source7e93c5d pushed and PR#76 opened/attached; watched automatic run41260496064 to8/8 successful OTP20–27 jobs and verified matching PR/run head plus actual test-step skips/successes. No additional source changes, merge, release or later-increment authority. Earlier local-only/pending-hosted-CI observations remain historical.

- v1.5 — 2026-10-31: Operator merged PR#76 (main1d85d0a, reviewed tree unchanged) and selected Project02 OTP28/29 CI before remaining source changes. P03-11 closes; all seven later repairs and final project gates remain open, with no automatic runtime advancement.

## CI checkpoint handoff — 2026-11-01 UTC

Project02 [PR#77](https://github.com/erlsci/iso8601/pull/77) at43a3740 passes all ten OTP20–29 jobs after the necessary private-spec and two parser-boundary compatibility fixes. Its249 tests/property/coverage results and147 local28 applicability observations preserve the two accepted interval repairs while all seven remaining finding rows stay open. The Operator merged it into main3a1cdfd with the exact reviewed tree; all ten automatic main-push jobs passed too. This related priority checkpoint is complete. No arithmetic/precision or parser-contract repair increment starts automatically.

- v1.6 — 2026-10-31: related CI checkpoint passed and merged with verified main integration; no next repair increment started. Current ledger notes for the already-merged first interval checkpoint are reconciled with P03-11.

## Next arithmetic/precision authorization — 2026-11-01 UTC

Operator: “nice! Let's jump into the next arc!” This directly authorizes the four-finding arithmetic/precision checkpoint, now sized as one coherent direct [Slice02](slice02-arithmetic-precision/slice-plan.md); no arc wrapper is needed. It supersedes the preceding no-next-increment state only for CAL001/CAL002/DATE003/DATE005. CDC gpt-6.1-sol High and fresh CC gpt-6-luna High continue in Autonomous Mode, one implementer plus independent CDC review and parent composition. Parent concurred with nonnegative arithmetic target floor/badarg, retained year0 target controls and original nonpositive parser offset/unit gate, fraction-preserving integer calendar path, separate private parser type and proleptic formatter carry. Public declarations/types/aliases remain unchanged. The source base is clean merged3a1cdfd/tree3224b1e, with the expanded OTP20–29 matrix already integrated; source worktree `.worktrees/project03-arc02-arithmetic`, branch `codex/project03-arc02-arithmetic`. Allowed source paths remain module/EUnit/README only. The exact active implementation recipe, reading/intake contract and validation are in Slice02; earlier first-slice worktree/toolchain descriptions remain historical.

The standing separate-PR workflow applies: parent publishes the independently prereviewed candidate and watches all ten exact-head jobs; CC does not push/PR/merge/release. Current four finding rows may close only after technical verification. New P03-14 independent parent composition and P03-15 human review/merge/next-work gates remain separate and open. P03-12/13 are whole-project gates; DATE001/002/004, known P02 type/tooling questions and all release decisions remain open. Durable new evidence lives only in Slice02 artifacts, preserving all original audit and first-checkpoint evidence.

- v1.7 — 2026-11-01 UTC: direct next-arc authorization activates collapsed Slice02 arithmetic/precision at integrated3a1cdfd; source-grounded four-finding policy and separate parent/human gates recorded before fresh CC dispatch.

## Release boundary — 2026-11-01 UTC

The Operator chose eventual release1.4.1 after all repairs land, in a separate release-preparation PR covering the version bump and release work. This is a future boundary and version decision, not authority to change version1.4.0, publish or release during Slice02. Current checkpoint remains arithmetic/precision only; three parser repairs and whole-project acceptance are open. No release-preparation increment has started.

- v1.8 — 2026-11-01 UTC: record the Operator's eventual1.4.1/separate release-preparation decision without extending the active repair checkpoint or changing sourceversion.

## Arithmetic/precision technical acceptance — 2026-11-01 UTC

[Slice02 verification](slice02-arithmetic-precision/cdc-verification.md) accepts CAL001/CAL002/DATE003/DATE005 at bd67bd0/tree72efe0dc. Independent native8 passes300 EUnit,4×100 PropEr,100% coverage; final tests against unchanged baseline fail39/300. All147 original audit observations were freshly replayed with current applicability, alongside10 numeric/interval integration and four raw-clock scope controls. Actual [PR78 run41267969674](https://github.com/erlsci/iso8601/actions/runs/41267969674) passes all ten OTP20–29 jobs;21–29 each300/4×100/100,20's static-only policy retained. Upstream OTP29 dependency warnings remain P02 tooling watch; no first-party diagnostic observed. The [retrospective](slice02-arithmetic-precision/cdc-retrospective.md) preserves ordering/evidence gaps, all five refinements and unestablished earlier remote-draft publisher rather than asserting complete process compliance.

The Operator explicitly said “merged!” and parent verified PR78 merged2026-11-01T03:11:41Z into maine332032561142bff6c620cd1e7b4a5bfe9929b0b. Parent owns final tree/integration/independent composition and P03-14/15 records; those rows remain parent-owned pending that attributable disposition. Three parser findings and whole-project gates remain open, future release1.4.1 preparation remains separate/unstarted, sourceversion1.4.0 unchanged, and no next increment begins automatically.

- v1.9 — 2026-11-01 UTC: four numeric finding rows close after independent native/hosted verification; PR78 Operator merge acknowledged without pre-filling parent composition/integration or whole-project acceptance.

Integration receipt for v1.9: parent API verifies synthetic PR merge and merged maine332032 have the reviewed tree72efe0dc; CDC local tree comparison independently agrees. Main push-run41268179012 independently inspected all10 step JSON successful (last job03:13:09Z), with prior fully inspected PR populations retained by tree equality. Parent final composition/human-row record remains separately authored.

## Parent composition and integrated checkpoint close — 2026-11-01 UTC

Parent [independent composition](slice02-arithmetic-precision/closing-report.md#parent-independent-checkpoint-composition--2026-11-01-utc) accepts the four-finding checkpoint after actual final source/API review, the same 16-control fixture against baseline and candidate (6/16 versus 16/16), public-declaration comparison, complete PR job/step/population inspection, and independently verified integration. Reviewed bd67bd0, synthetic PR merge e5e5069, and merged main e332032 all have tree `72efe0dc781ed31cdac78872670465b92940da80`. All ten PR and main CI jobs pass; the clean primary checkout is fast-forwarded to the user's merge. All five rebased source commits retain the required adjacent coauthor trailers.

The Operator's explicit “merged!” and GitHub's attributable merge receipt complete this checkpoint's human merge gate. P03-14 and P03-15 are done/reproduced; this supersedes their earlier parent-pending state. DATE001/002/004 and P03-12/13 remain open. The next parser increment has not started and still requires the next go-ahead. Future 1.4.1 release preparation remains a separate PR after all repairs land; source version 1.4.0 is unchanged. Project02's wider audit and upstream tooling watch are also still open.

- v1.10 — 2026-11-01 UTC: parent independently composes the accepted numeric repair, verifies exact-tree user integration and ten-job main CI, and closes P03-14/15 without accepting the remaining parser, audit, project or release work.

## Final parser checkpoint authorization — 2026-11-01 UTC

Operator: “let's dive into the next arc!” CDC/parent concurred on one direct [Slice03](slice03-parser-contracts/slice-plan.md) for DATE001/DATE002/DATE004 only, using clean mergede332032/tree72efe0dc, source `.worktrees/project03-arc03-parsers` on `codex/project03-arc03-parsers`. Actual CDC GPT-6.1 Sol High / fresh actual CC GPT-6 Luna High continue Autonomous Mode; first CC intake must STOP after bounded full manifest/readback for explicit CDC clearance before source/test/README edits. Then real tests/red precede production repair. All four public iodata declarations are honored, existing duration string contracts and sixteen baseline type declarations preserved, raw fields validated before normalization using proleptic rules and actual ISO year52/53 count. Existing nonpositive offsets/negative weeks and unrelated clock behavior remain outside the repair. Eight native gates/100% coverage, all147 current applicability observations, independent review/retrospective and actual ten-job CI remain mandatory. Parent alone publishes the reviewed separate PR/watches CI; no CC external actions.

New P03-16 composition and P03-17 human review/merge/next-work rows apply to this checkpoint. P03-12/13 final-project gates remain open even after all nine findings are technically repaired. This does not automatically close the remediation project, complete wider P02 audit/type/tooling work or open eventual1.4.1 release preparation. Previous checkpoint originals/retrospectives and unknown earlier remote-draft provenance remain preserved, with no new forensic inquiry.

- v1.11 — 2026-11-01 UTC: direct next-arc authorization opens coherent Slice03 for the last three parser findings; source-grounded four-boundary design, explicit pre-edit intake stop and separate third-checkpoint/project/release gates recorded before CC dispatch.

## Parser checkpoint technical acceptance — 2026-11-01 UTC

[Slice03 independent verification](slice03-parser-contracts/cdc-verification.md) accepts DATE001/DATE002/DATE004 at22b92d1/tree643ed335: raw date/week validation and FOUR byte-iodata boundaries, all16 baseline type declarations/15 public specs/15exports/11exportedtypes unchanged. Independent eight native gates pass430EUnit/4×100PropEr/100%coverage; current430tests fail88 against unchanged e332 (85 target-behavior failures plus3 outside-contract invalid-interval reason changes). CC's earlier421-test87/334 red remains true at its own pin. All147 immutable observations were freshly replayed with current nine-finding applicability; six prior repairs retain their expected results. Actual PR79run41271243883 passes ten OTP20–29 jobs,20staticonly and21–29each430/4×100/100. Upstream dependency warnings and wider P02 type/tooling/audit obligations remain open. The [retrospective](slice03-parser-contracts/cdc-retrospective.md) retains intake/oracle/citation/provider-interruption chronology without a model or methodology effectiveness claim.

The Operator explicitly said “merged!”; parent verifies PR79 merged by oubiwann04:01:43Z into main10a182be1bf4f5cd532592558e87789b83edb96f. Parent owns final exact-tree/mainCI/independent composition and P03-16/17 records. P03-07/08/09 are technically done/reproduced; all nine finding criteria now have repair evidence. Whole-projectP03-12/13 remain OPEN even after the child findings and human merge: project-scale synthesis/acceptance is a distinct later decision. No automatic next work or release preparation; sourceversion1.4.0 and separate future1.4.1 preparation boundary remain.

- v1.12 — 2026-11-01 UTC: the final three parser finding criteria close after independent native/hosted validation; attributable human merge acknowledged while parent final composition/integration and whole-project gates stay separately owned.

Integration receipt for v1.12: CDC locally resolves merged main10a182 to the exact reviewed/synthetic tree643ed335 and independently inspects all ten main-run41271453140 job/stepJSON conclusions. All configured steps succeed with only20's intended3skips; prior fully inspected PR430/4×100/100% populations remain applicable by tree equality, with no full main-log reread claimed. Parent owns attributable final composition/P03-16/17 disposition; whole-project12/13 stay open.
