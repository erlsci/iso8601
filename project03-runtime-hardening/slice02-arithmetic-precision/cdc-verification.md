# Slice02 CDC independent verification

Status — 2026-11-01 UTC: **local technical prereview reproduced; approved for parent publication and actual OTP20–29 CI. Hosted acceptance and final CDC close remain open.** CDC is GPT-6.1 Sol High; implementation/self-review is CC GPT-6 Luna High. Parent composition and Operator merge are separate gates.

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

Nine opening rows remain nine rows. S02-01 is reproduced as recovered intake/readback before resumed repair, with the original violation retained; this is not a claim of initial ordering compliance. S02-02/03/04/05/06/07 are locally done/reproduced by the independent evidence above. S02-08 remains open until actual final-head ten-job hosted logs are inspected; S02-09 remains open for final CDC acceptance/retrospective. The four project finding rows remain open pending that hosted gate. Parent P03-14 and Operator P03-15 are not accepted by this record.

Scope as specified and delivered is the same four-finding numeric checkpoint. The private carry provenance, four missing literal formatter controls, two private type-argument corrections and removal of an inaccurate new private spec complete that contract; they do not authorize parser validation or annotation cleanup. All five permitted refinements are consumed. Any further material blocker needs an explicit disposition rather than an unrecorded sixth iteration. Source ownership is released to parent for independent16-control composition and the authorized separate PR/ten-job CI; CDC retains this planning subtree for final evidence and close. No additional source changes are proposed.

## Retained limitations and process evidence

The CC edited before complete required reading/readback and red proof. The initial unvalidated commit e64c20f is retained; intake recovery and subsequent red/repair chronology are recorded, not backdated. The first stale-build147 replay is invalid and superseded by fresh isolated replay. An early local private datetime-spec Dialyzer warning lost its original raw output/exit; only contemporaneous attestation and clean reruns survive. An unrelated P02 warning was rejected as a substitute. This gap remains disclosed even though final independent Dialyzer passes. Tool-wrapper/equality-query errors are retained as checker errors rather than product failures. The final retrospective will distinguish these process failures, actual carry correctness defects and late author/reviewer private-spec omissions without causal model claims.

Artifact inventory: immutable prompts01–05 and original prompt; slice plan/ledger; CC closing report; CC runlog and native/red/replay/equality receipts under artifacts; this CDC verification and five independent native/replay logs. Final hosted receipt and CDC retrospective are still to be added. Raw logs remain byte-preserved, including terminal whitespace. This prereview planning commit is identified by the subsequent attributable parent handoff; no self-referential hash is fabricated.

Prereview metadata correction: the initial9c49742 prose mistakenly carried the older audit function population74/80. The actual independently captured final comparison is80/86 after the prior interval helper additions. This corrects that denominator only; the12 changed bodies/six new helpers and declaration equality assertions already match the raw receipt.

## Publication and provenance observation — 2026-11-01 UTC

Parent published reviewed bd67bd0 and created [PR#78](https://github.com/erlsci/iso8601/pull/78), with [ten-job run41267969674](https://github.com/erlsci/iso8601/actions/runs/41267969674) underway. Parent reports independent16-control composition and external-declaration comparisons pass; these are attributable parent results, not CDC reruns.

The authorized parent push unexpectedly reported remote advancement e64c20f→bd67bd0. Shared remote-tracking reflog independently shows e64 at2026-10-31 21:35:57-0500 and bd67 at22:08:20, each “update by push”; the original e64 local commit was21:31:01. Thus an earlier local Git push occurred. Its issuing context/tool is not established. CC reports no explicit push/publication command in her recorded activity; CDC likewise has no recorded push. Both currently find no core.hooksPath override or non-sample common hooks; this does not prove historical configuration. The earlier command/output sufficient to rule out a wrapper is unavailable. Blanket historical no-publication assertions are therefore not independently certified. Preserve this observed discrepancy and unknown provenance without blaming a contributor or inventing a cause. The reviewed final head and its test evidence are unaffected. No further forensic investigation or source changes are authorized through this observation.
