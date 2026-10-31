# Slice02 CC close — calendar and interval contracts

Date: 2026-10-31. Current assignment: `cc-prompt-iteration02.md`, planning base `6314444f7c20a39116d2493fcbc19f28e4b415ed`. Source: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit`, `codex/project02-arc01-audit`, `bcfd94a08fa7a427eb1469679897e4d92d808edf`; source SHA256 and clean status are in `artifacts/runlog.md`. CC submits the refined Slice02 diagnosis and runtime index as proposed evidence; CDC’s independent verification and the Operator’s arc gate remain open.

## Independent CDC disposition — 2026-10-31

Accepted at slice scope after actual source review, independent38/64-call reproduction and metadata correction12ca2cd. Allseven ledger rows are done/reproduced; see [`cdc-verification.md`](cdc-verification.md) and [`cdc-retrospective.md`](cdc-retrospective.md). CC's historical attestation below is retained with its original responsibility boundary. Arc composition and Operator decision remain open.

## Correction02 current close

Correction02 changes semantic ownership and terminology only. The runtime source and inputs are unchanged, so no probe or native gate was rerun. CDC’s independent byte-identical reproduction of the 64-call refinement01 transcript remains reviewer-owned prior evidence, not a new CC run.

The current EPP logical map is **38 date + 14 arithmetic/duration + 22 interval = 74 forms**, with70 names,15 exports and zero residual. `gi/1@151` is arithmetic/duration because its only actual callers are the duration captures at181–186; date `acc/5@390` instead calls `list_to_integer/1`. The earlier39+13+22 grouping remains preserved as historical in the prior runlog and candidate commit, and is explicitly superseded by the corrected index/readback. S02-04 remains CC-attested pending CDC verification.

The current clean-check10 includes only successful controls: year1 Jan−12 months and year1 Jan−1 year both return year0 Jan, while installed-calendar validity is `{true,false,false}`. The two silent year-zero December wraps remain failure evidence under CAL001, outside the clean-check list. Zero-duration controls are integer-second `datetime` values; the ordinary `parse_exact/1` anchor is a float-zero `datetime`. Both have the date/time two-tuple shape, unlike the distinct three-tuple `timestamp()` type; retained EUnit timestamp-dispatch wording remains accurate.

No finding IDs, severity grades or totals changed: four Slice02 findings (3 High, 1 Medium) and nine Arc01 findings (3 High, 5 Medium, 1 Low). S02-07 remains open for CDC verification/retrospective and parent/Operator gate. Source stays unchanged; no Arc02 work or external action occurred.

## Refinement01 close (historical before correction02)

The required refinement is complete, diagnosis-only, and proposed for independent review. The prior close packet below remains preserved as the initial candidate at `cb26ad8588d2a96932bd2caef3867c1b3e2717b5`; its original 38-case transcript remains unchanged and CDC independently reproduced it byte-identically. Refinement01 adds a separately named runner and full 64-case transcript.

### Current artifact inventory

- `artifacts/2026.10.31-audit-results-erlang-calendar.md` — four stable IDs, calibrated severities, R1–R4 evidence/disposition, scope and limits.
- `artifacts/calendar_interval_probes_iteration01.erl` and `artifacts/calendar-interval-probes-iteration01.out` — fresh runner and complete 282-line OTP28 stdout, including monitored hangs and stack capture.
- `artifacts/2026.10.31-audit-index.md` — all15 exports and74 EPP forms/70 names retained, updated case crosswalk and severity totals.
- `artifacts/runlog.md` — original intake/full-read receipts, refinement readback, new commands/results, severity reasoning and prior-vs-fresh attribution.
- This `closing-report.md` and evidence/status cells in `ledger.md`.

No issued prompt, plan, CDC record, Slice01 artifact/history, or tracked source/test/config was changed. Original runner/output at `cb26ad8` remains available as historical evidence.

### Current ledger row walk

- **S02-01 — CC attested:** the issued iteration01 prompt, CDC review, required readings, full Erlang-guide receipts carried from initial intake, fresh pins and contract readback are recorded in `artifacts/runlog.md` before refinement probes.
- **S02-02 — CC attested:** all ten assigned exports and shared-helper paths remain mapped; 64 fresh labeled calls include multiple clean controls and the required zero-duration behavior. No test-quality or whole-project claim is added.
- **S02-03 — CC attested:** four stable findings now have source locations, severity rationale, exact expected/actual/control evidence, and calibrated confidence. CAL001 includes both silent floor wrap and monitored negative-year nontermination; CAL002 includes ordinary parse-produced float-zero input and captured stack; CAL003 uses formatter-produced `+2007`; CAL004’s ordinary two-hour consequence is separated from medium-high repository-intent confidence. All suspected hangs are in monitored workers with timeout, kill and observed `DOWN`.
- **S02-04 — CC attested:** the retained complete EPP map is still74 forms/70 names/15 exports with zero residual; index crosswalk is updated. Stable Arc01 total is9: 3 High, 5 Medium, 1 Low. CAL002–004 are High; CAL001 remains Medium with explicit floor-boundary/policy rationale. No candidate is counted.
- **S02-05 — CC attested:** fresh isolated OTP28 source/runner compile and all64 calls are retained in the iteration01 transcript. Initial Slice02 38-case output and CDC byte-identical rerun remain separately attributed at `cb26ad8`. Slice01 native gates are retained, not rerun; supported OTP matrix, R15B03 and native project gates remain unclaimed.
- **S02-06 — CC attested:** source checkout remains clean and byte-identical at the pinned revision. Intended planning changes stay within the permitted Slice02 artifacts, closing report and ledger evidence/status fields.
- **S02-07 — OPEN for CDC:** CDC must independently verify the current candidate, reproduce new/affected findings, account for rows and bubble-up, and complete its reviewer-owned verification/retrospective. Parent/Operator arc gate remains separate.

### Findings, zero durations and bubble-up

Current Slice02 IDs remain `ERL-CAL-001`–`004`: one Medium and three High. With accepted Slice01’s four Medium and one Low, the stable Arc01 total is **9 findings: 3 High, 5 Medium, 1 Low**. Slice01 IDs are not duplicate-counted. Severity follows guide02’s realistic-operating-risk definition; CAL004 is High for the ordinary two-hour bounds error against the documented/preserved inheritance intent, while its evidence confidence remains medium-high because the README does not name timezone separately and the archived resolved intent is not ISO authority.

The R1 month controls freshly show year0-Jan−1 and year1-Jan−13 both silently return year0-Dec; year1-Jan−12 is the valid year-zero floor control. R2 freshly shows ordinary `parse_exact("2017-01-01")` yields seconds `0.0`, applying `P1D` crashes through `calendar:time_to_seconds/1` line817 / `calendar:datetime_to_gregorian_seconds/1` line259 / `iso8601:apply_offset/4` line499, and an integer-second matched control succeeds. R3 freshly starts from `format_expanded/1`’s `+2007` output, accepted by `parse/1`, and shows inherited year200 plus explicit-end `badarg`; the seven-digit signed discriminator remains too. R4’s `P0D`, `PT0S` and bare `P` controls parse, apply, bound and serialize consistently with historical zero-component omission/always-P intent; no defect is manufactured. Existing timestamp-dispatch EUnit lines334–341 are cited as retained evidence, not a fresh test run.

The reduced-precision zoned-start `badarg` remains a real Arc02/parser-scope candidate: full-seconds succeeds, but the current contract does not promise this reduced precision with offset and parsing fails before inheritance. The `parse_exact/1` public return spec mismatch remains an Arc02 obligation. Arc01 scope/order and the last-slice boundary do not change; no Arc02 work began. No source repair, issue, push, PR, merge, release, publication or external message occurred.

## Initial candidate close (historical; committed at cb26ad8 and superseded above)

## Disposition and artifact inventory

Created or updated only within this slice’s authorized paths:

- `artifacts/2026.10.31-audit-results-erlang-calendar.md` — four source-cited Medium findings, a separately dispositioned parser candidate, clean checks, scope and limits.
- `artifacts/calendar_interval_probes.erl` and `artifacts/calendar-interval-probes.out` — reproducible bounded public-API runner and full output, compiled/run on the fresh isolated OTP28 beam.
- `artifacts/2026.10.31-audit-index.md` — all15 exports, all74 EPP function forms, complete helper grouping, stable deduplicated count and Arc02 boundary.
- `artifacts/runlog.md` — actual assignment/read receipts, source-cited intake, prior evidence attribution, setup failures, fresh commands/output, EPP receipt and limitations.
- This `closing-report.md` and evidence/status updates to `ledger.md`.

No issued prompt, plan, Slice01 artifact, reviewer record, historical archive, or tracked source/test/config was changed. The invalid first escript harness was removed after its setup failure was recorded; the retained ordinary Erlang module is the executable probe source.

## Slice ledger row walk

- **S02-01 — CC attested:** assignment/source pins, complete required manifest extents, source-cited readback and current environment are recorded before fresh evidence in `artifacts/runlog.md`.
- **S02-02 — CC attested:** all10 assigned APIs and shared helper paths have report/index mappings and public observations; report lists more than five concrete clean checks and explicit logical-unit/module/library scope.
- **S02-03 — CC attested:** four findings have severity/scale/location/expected-vs-actual/control/limits and reproducible public probes. Underflow claims use four monitored timeouts with kill/`DOWN` plus installed-calendar/source termination invariant. The minute-precision zoned-start candidate is separately dispositioned without a standards claim.
- **S02-04 — CC attested:** fresh `epp:parse_file/3` returned74 function forms,70 names,15 exports; disjoint helper groups sum39+13+22=74 with residual0. Index maps all15 exports and composes accepted Slice01 findings without duplicate IDs.
- **S02-05 — CC attested:** focused fresh compile/probes and exact outputs are retained. Accepted unchanged Slice01 native checks are identified as prior CC/CDC evidence, not rerun; formatter/linter failures and OTP/alias limits remain explicit.
- **S02-06 — CC attested:** source remains pinned and unchanged; commit scope is only the above Slice02 evidence, close and ledger. No source repair or external action is included.
- **S02-07 — OPEN for CDC:** this is CC’s close packet, not independent acceptance. CDC must check artifact/source links and counts, independently rerun the focused probes/termination proof, disposition the rows, and add its reviewer-owned verification/retrospective. Arc composition remains with the parent and Operator gate.

## Validation, findings and retained evidence

Fresh Slice02 findings: **4 Medium**, IDs `ERL-CAL-001`–`004`. Combined with the accepted Slice01 report: **9 stable IDs total (8 Medium, 1 Low)**; candidate disposition is excluded. Fresh probes use a newly compiled beam from the pinned unchanged source. The capture includes all four interval forms, both separators, bounds and format calls, arithmetic/duration controls, expanded-year/zone/fractional-anchor discriminators, and malformed controls. Underflow workers are monitored and cleaned up. Full exact values are in the transcript.

Retained prior evidence only: Slice01’s source-pinned OTP28 compile/xref/Dialyzer/EUnit210/PropEr4×100/coverage100% outputs and CDC independent reruns. Formatter/linter plugin update failures remain in the earlier runlog. No Slice02 full native-gate rerun occurred because native inputs are unchanged; no supported OTP20–27/R15B03, aggregate alias, or project-wide acceptance is asserted.

## Bubble-up and boundaries

Arc01 ordering and scope do not change. Slice02 completes the assigned runtime-diagnosis handoff as CC-proposed evidence and is the last slice in this authorized arc; independent CDC verification and Operator review are now the required gate. No Arc02 work started.

Four findings record concrete current behavior: month/year underflow clamp recursion cannot reach a valid negative-year date; duration composition fails on a `datetime()` float-second anchor; interval parsing mishandles a documented standalone signed positive expanded year; abbreviated time inheritance drops the start offset, supported by README’s component-inheritance promise and the preserved resolved interval intent. The latter is calibrated as repository intent, not external standards proof. The reduced-precision zoned-start `badarg` is real but occurs in the parser before inheritance and lacks a current project promise, so it is retained as a separate Arc02/parser-scope candidate, not a finding. Slice01 DATE002/DATE005 are not recounted. The `parse_exact/1` public return-type mismatch remains uncleared for Arc02, along with complete type/tooling reconciliation.

Arc02 remains responsible for the test/property quality audit, coverage escript, build/tool/package and supported-OTP compatibility, public type reconciliation, historical-claim reconciliation against authoritative sources, and final complete-language report/index/synthesis. No source edits, remediation tickets, push, PR, merge, release, publication, or external messages were made or authorized.
