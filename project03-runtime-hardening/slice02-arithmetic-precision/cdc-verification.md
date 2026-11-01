# Slice02 CDC independent verification

Status — 2026-11-01 UTC: **CDC technical acceptance reproduced for all nine slice rows at bd67bd0; all ten hosted jobs passed. PR#78 subsequently merged by Operator; parent integration/composition record follows separately.** CDC is GPT-6.1 Sol High; implementation/self-review is CC GPT-6 Luna High. Parent composition and Operator merge are separate gates.

## Candidate and independent evidence

Reviewed source `bd67bd0d4306f9377bf617f802616b342fd9ee77`, tree `72efe0dc781ed31cdac78872670465b92940da80`, against main `3a1cdfd17bd4dfaf36c9eaa34c5102944a3e0828`. Final CC planning handoff `f2edd25f4bf07deb9356fc3dd2358ca0194ea743`; issued refinement05 and plan v1.5 govern this return. Source is clean; only README.md, src/iso8601.erl and test/iso8601_tests.erl differ from baseline (424 insertions/32 deletions). All five reachable source commits have the required adjacent Codex/Billo trailer block. No merge/release by CDC or CC is recorded. An earlier unvalidated remote draft was discovered during publication; its writer is unresolved, as recorded below.

CDC read the actual implementation, tests, README, all five refinements, final CC report and relevant runlog chronology. [Independent native transcript](artifacts/cdc-native-validation.log) records final head, file hashes, exact commands/exits, timings, isolated fresh compilation, EPP comparison, red comparator and exact integration outputs. Locally observed OTP28.1.1/ERTS16.1.1/rebar3.27.0; owned default/test project modules were cleaned before final native gates. Dependency caches and the PLT were retained, not represented as fresh installations.

| Final-source native command | Independent result |
|---|---|
| rebar3 compile | exit0 |
| rebar3 xref | exit0 |
| rebar3 dialyzer | exit0, no warning emitted |
| rebar3 eunit -c -v | exit0;300 passed |
| rebar3 proper -c | exit0;four properties ×100 cases |
| rebar3 as test cover -v --min_coverage=100 | exit0;module and total100% |
| rebar3 as dev fmt --check | exit0 |
| rebar3 as dev lint | exit0 |

This is eight native commands, not an aggregate make/check assertion or a reproduction of the supported OTP matrix. Fresh baseline production compiled with the same final300-case test module yields261 passed/39 failed/0 skipped, exit1; final candidate passes300. This independently confirms meaningful red/green behavior. The first draft preceded the original red runs; later comparator evidence does not retroactively establish red-before-first-edit.

Fresh isolated final-source beams replayed all147 original observations: [31 date](artifacts/cdc-replay-date-31.log), [10 fractional-unit](artifacts/cdc-replay-unit-10.log), [42 calendar](artifacts/cdc-replay-calendar-42.log), [64 refinement](artifacts/cdc-replay-calendar-refined-64.log). Only embedded beam directory was replaced in the two temporary escript copies, with reverse reconstruction checked; the two module runners were copied unchanged. All four full stdout files are byte-identical to CC's final runtime receipts. All10 unit comparisons are true within1e-12. The eight bounded underflow observations across the two calendar runners return badarg without timeout. Original audit scripts/reports remain unchanged. Their print-runner exit0 alone is not a semantic oracle: the following dispositions depend on actual values, source boundaries and independent literal-valued tests.

## Nine findings and scope disposition

| ID | Local independent disposition | Concrete evidence / boundary |
|---|---|---|
| CAL001 | Repaired locally; hosted pending | Explicit month-index/day/second/year floor checks precede conversion; bounded month-end clamp replaces recursion. Both silent month-wrap inputs reject badarg, four hang families reject promptly in each runner; valid year0 targets, leap clamp and signed durations survive. |
| CAL002 | Repaired locally; hosted pending | Integer epoch conversion plus separately retained small fraction composes ordinary0.0,0.5 and fine large-year anchors with shifts, durations and backward bounds. No float epoch sum. Final tests and both calendar transcripts distinguish integer/float shapes. |
| CAL003 | Prior repair retained | Actual formatter-produced +2007 plus abbreviated end retains2007; signed extended-year/date-crossing composition below also passes. |
| CAL004 | Prior repair retained | Abbreviated ends inherit raw start zone; explicit endpoint controls survive. New exact fractional-unit + signed-year + zone integration preserves correct UTC bounds. |
| DATE001 | Open, later parser checkpoint | Invalid week0, 2014 week53 and weekday8 still normalize in date transcript. |
| DATE002 | Open, later parser checkpoint | Invalid negative/year0 calendar dates still return tuples. No scope broadening to raw malformed clocks. |
| DATE003 | Repaired locally; hosted pending | Both float formatters round bounded seconds to integer microseconds and carry through minute/hour/month/year/negative→0/0→1/9999→10000 with literal binaries. Non-midnight controls and below-carry controls discriminate a midnight-only or unconditional carry implementation. |
| DATE004 | Open, later parser checkpoint | Nested iodata still raises badarg in all three public parsers. |
| DATE005 | Repaired locally; hosted pending | All10 immutable exact-unit probes now match; exact hour0.0001 gives0.36 seconds and minute0.001 gives0.06. Legacy fractional-unit rounding and explicit fractional-second truncation stay distinct. Original nonpositive-year rounded-unit/zone gate stays unchanged. |

CDC additionally executed10 exact/tolerance-qualified integration controls recorded in the native transcript: exact-unit parsing→P1D→six-digit formatting; signed +10000 raw-local start plus inherited zoned fractional end across9999/10000; duration/end backward bounds with that anchor; negative-year exact represented carry→expanded formatting into year0; and year0 February month clamp retaining0.25. Fractions are compared to independent0.36/0.72 literals within1e-12, serialized binaries exactly. All10 pass. Four baseline/final raw negative/year0 clock observations (hour25 or raw second60) are byte-identical; valid-base provenance prevents new carry logic from normalizing those unrelated malformed forms.

Independent annotation-normalized EPP comparison confirms unchanged15 function exports,11 exported types, all14 original type declarations and15 public specs. New parser-mode/accumulator types remain private and do not widen duration's alias. Baseline80/candidate86 function forms; exactly12 expected original bodies change, six private helpers are added, and all other original bodies are unchanged. Structural equality protects external declarations, not the correctness of every private annotation. CDC separately inspected the two make_date accumulator arguments and removal of the newly inaccurate add_decimal spec. The pre-existing parse_exact return-spec mismatch, reduced-precision zoned START, and broader tooling/history audit remain P02 obligations. App version1.4.0, dependencies, CI20–29 and intended syntax floor are untouched.

## Row walk and publication boundary

At publication prereview, nine opening rows remained nine rows. S02-01 was reproduced as recovered intake/readback before resumed repair, with the original violation retained; this is not a claim of initial ordering compliance. S02-02/03/04/05/06/07 are locally done/reproduced by the independent evidence above. S02-08 remains open until actual final-head ten-job hosted logs are inspected; S02-09 remains open for final CDC acceptance/retrospective. The four project finding rows remain open pending that hosted gate. Parent P03-14 and Operator P03-15 are not accepted by this record.

Scope as specified and delivered is the same four-finding numeric checkpoint. The private carry provenance, four missing literal formatter controls, two private type-argument corrections and removal of an inaccurate new private spec complete that contract; they do not authorize parser validation or annotation cleanup. All five permitted refinements are consumed. Any further material blocker needs an explicit disposition rather than an unrecorded sixth iteration. Source ownership is released to parent for independent16-control composition and the authorized separate PR/ten-job CI; CDC retains this planning subtree for final evidence and close. No additional source changes are proposed.

## Retained limitations and process evidence

The CC edited before complete required reading/readback and red proof. The initial unvalidated commit e64c20f is retained; intake recovery and subsequent red/repair chronology are recorded, not backdated. The first stale-build147 replay is invalid and superseded by fresh isolated replay. An early local private datetime-spec Dialyzer warning lost its original raw output/exit; only contemporaneous attestation and clean reruns survive. An unrelated P02 warning was rejected as a substitute. This gap remains disclosed even though final independent Dialyzer passes. Tool-wrapper/equality-query errors are retained as checker errors rather than product failures. The final retrospective will distinguish these process failures, actual carry correctness defects and late author/reviewer private-spec omissions without causal model claims.

Artifact inventory: immutable prompts01–05 and original prompt; slice plan/ledger; CC closing report; CC runlog and native/red/replay/equality receipts under artifacts; this CDC verification and five independent native/replay logs. Final hosted receipt and CDC retrospective are still to be added. Raw logs remain byte-preserved, including terminal whitespace. This prereview planning commit is identified by the subsequent attributable parent handoff; no self-referential hash is fabricated.

Prereview metadata correction: the initial9c49742 prose mistakenly carried the older audit function population74/80. The actual independently captured final comparison is80/86 after the prior interval helper additions. This corrects that denominator only; the12 changed bodies/six new helpers and declaration equality assertions already match the raw receipt.

## Publication and provenance observation — 2026-11-01 UTC

Parent published reviewed bd67bd0 and created [PR#78](https://github.com/erlsci/iso8601/pull/78), with [ten-job run41267969674](https://github.com/erlsci/iso8601/actions/runs/41267969674) underway. Parent reports independent16-control composition and external-declaration comparisons pass; these are attributable parent results, not CDC reruns.

The authorized parent push unexpectedly reported remote advancement e64c20f→bd67bd0. Shared remote-tracking reflog independently shows e64 at2026-10-31 21:35:57-0500 and bd67 at22:08:20, each “update by push”; the original e64 local commit was21:31:01. Thus an earlier local Git push occurred. Its issuing context/tool is not established. CC reports no explicit push/publication command in her recorded activity; CDC likewise has no recorded push. Both currently find no core.hooksPath override or non-sample common hooks; this does not prove historical configuration. The earlier command/output sufficient to rule out a wrapper is unavailable. Blanket historical no-publication assertions are therefore not independently certified. Preserve this observed discrepancy and unknown provenance without blaming a contributor or inventing a cause. The reviewed final head and its test evidence are unaffected. No further forensic investigation or source changes are authorized through this observation.

## Final hosted acceptance and row disposition — 2026-11-01 UTC

CDC independently inspected the complete parent-fetched803469-byte log (SHA256 `e29eb6b64c49fe508bc1355662371b1e8fb595a41c7ca7af41be9a32a5d013fa`) and [complete ten-job/run JSON](artifacts/hosted-run-41267969674.json), not only the run conclusion. [Bounded raw receipt](artifacts/cdc-hosted-validation.log) preserves identities, each job URL, observed versions, every gate population, selected actual semantic test results, warning context and full-log hash/location. A one-off grouped-log inspection asserted unique exact majors20–29, all completed/success, compile/xref/Dialyzer success for all10, and only OTP20's three test skips. EACH21–29 job has one300-EUnit summary, four100-case PropEr summaries and module+total100% coverage rows. Run41267969674 completed2026-11-01T03:10:13Z associated with headbd67bd0. OTP28.5.0.8/ERTS16.4.0.7 and29.1.2/ERTS17.1.1 use rebar3.28.0; actual older pins remain3.15.2/3.22.1/3.25.1. Local final fmt/lint are independent of hosted gates; CI does not add those providers.

Six warning renderings on OTP29 represent four upstream source sites: rebar3_proper_prv.erl84/166 deprecated catch, proper_statem.erl770 deprecated catch and proper_erlang_abstract_code.erl2138 overridden is_record/1. The latter two each render twice. No first-party warning was observed in the inspected final native/hosted output. This is not a warning-free dependency claim. Retain existing P02 tooling watch; no suppression/dependency update or extra audit is inferred.

| Row | Final CDC disposition | Independent evidence |
|---|---|---|
| S02-01 | done/reproduced, recovered intake only | Actual post-draft readback and bounded read receipts reviewed; initial ordering breach retained. No retroactive compliance claim. |
| S02-02 | done/reproduced | Fresh bounded floor replay, exact year0 controls, baseline red/final300 tests and actual21–29 execution. |
| S02-03 | done/reproduced | Fine/large-year/float-zero/duration/backward-bound literal tests,10 integration controls, fresh replay and hosted300. |
| S02-04 | done/reproduced | All10 exact-unit matches plus independently reviewed legacy, nonpositive-year and no-carry controls; native/hosted tests. |
| S02-05 | done/reproduced | Exact formatter binaries through non-midnight and proleptic boundaries, parse/interval integration, baseline red and native/hosted300. |
| S02-06 | done/reproduced | Full three-path diff, unchanged15/11/14/15 public declarations,80→86 forms/12 changed/six new private helpers, README review, all147 current applicability observations and four raw-clock comparisons. |
| S02-07 | done/reproduced | Fresh final eight native gates,300 EUnit,4×100 PropEr,100% coverage; exact logs/exits above. |
| S02-08 | done/reproduced | Actual ten-job PR run with all step/population checks and retained OTP20 skips; JSON and independent raw receipt above. |
| S02-09 | done/reproduced | This independent row walk, source/diff/artifact review and [CDC retrospective](cdc-retrospective.md); parent/human-scale acceptance not inherited from it. |

Nine opening rows/nine final dispositions; no dropped, deferred or no-op slice criterion. The four numeric finding rows are technically accepted at this candidate. Three parser finding rows and whole-project/P02 obligations remain open. Scope-as-delivered matches the specified four-finding checkpoint; release version remains1.4.0. The artifact inventory above is completed by the hosted JSON/receipt and retrospective; no supporting verifier product or new future-slice tree was created. All issued prompts and historical/failed/invalidated receipts remain intact.

The Operator then explicitly said “merged!”; parent verified PR#78 merged by oubiwann2026-11-01T03:11:41Z into maine332032561142bff6c620cd1e7b4a5bfe9929b0b. Final candidate source pins remain immutable. Parent owns the independent synthetic-merge/final-main tree verification, automatic integration-run observation and P03-14/15 disposition; this child record does not prefill those checks. No next parser increment or release starts from the merge decision. Technical source implementation is complete; all source/planning ownership will be released to parent at the attributable final planning commit.

### Integrated tree and main-run receipt

Parent's GitHub API check identifies actual PR synthetic merge `e5e50690975c48c963ff4f42b908e8d223af4ba4`, parents3a1cdfd/bd67, tree `72efe0dc781ed31cdac78872670465b92940da80`. CDC independently read locally fetched origin/main and candidate tree IDs: both that exact tree; `git diff --quiet bd67bd0 origin/main` exits0. Thus PR checkout/source candidate/integrated main have identical complete contents despite different commits. Native/runtime/PR population evidence retains applicability without relabelling source pins.

CDC independently inspected [main push-run41268179012 JSON](artifacts/hosted-main-run-41268179012.json): exact heade332032, completed/success, unique majors20–29, all10 jobs and intended steps successful, only20's test trio skipped; last job2026-11-01T03:13:09Z. [Hosted receipt](artifacts/cdc-hosted-validation.log) records the JSON hash and assertion limits. No redundant full main log was fetched and no separate main300/property/coverage populations were counted; the full PR log populations retain applicability by exact tree equality. Parent owns the final composition/human project-row dispositions; merged/integrated status is now verified, while next parser/release decisions remain open.
