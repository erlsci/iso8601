# CDC verification — Slice01 date contracts

## Initial attempt review — 2026-10-31

Reviewer: CDC, OpenAI GPT-6.1 Sol High, independent of CC Luna High. Assignment `cc-prompt.md`, CC planning candidate `3dc34cf1c815c2df579505e3570590303cfe3e8c`; unchanged source `bcfd94a08fa7a427eb1469679897e4d92d808edf`. **Verdict: return for refinement; slice not accepted.** Current assignment becomes `cc-prompt-iteration01.md`.

Read actual report, probe source/output, full runlog and close/ledger against source/public exports and preserved prompt. Independently reran compile, xref, Dialyzer, EUnit210, four PropEr properties×100, coverage100%; all pass locally on OTP28. Independently repeated format/lint: both exit1 after plugin update failures/command unavailable. Native output and a rerun of every printed public probe are preserved separately in reviewer-labelled sections of `artifacts/runlog.md`. Retained fresh native dependencies/OTP28 PLT, not a new dependency resolution or supported-OTP matrix. Source no-diff check passes.

The four original findings' underlying public observations reproduce. I independently checked cited official OTP18 calendar passages (Gregorian/year0/valid-date contract, ISO week/week-number interface), NIST leap-second page, IERS Bulletin C metadata, and iodata-related OTP documentation on2026-10-31. DATE001–003 are reasonable Medium malformed-input/rounding-boundary diagnoses; DATE004 is Low public-spec mismatch. This supports their current observations, not an accepted complete report. All intended <=0/nonzero-offset rejection stays excluded as historical design intent. No source correction was made.

### Required corrections within existing scope

- **R1 — fractional-unit scope gap (S01-02, S01-03, S01-06):** the report/close claims every fractional helper addressed but probes lack fractional-hour/minute subsecond cases. CDC independently observed `parse_exact("2012-02-03T04.0001")` -> second0.0 versus arithmetic0.36; `T04.0002` ->1.0 versus0.72; `T04:05.001` ->0.0 versus0.06. `.25` hour and explicit`.36` seconds controls pass. Source `add_decimal/2`429–432 calls `apply_offset/4`494–500, which rounds fractions before the exact parser's separately retained seconds fraction is added. Preserve uncertainty about legacy behavior; inspect intended exact/microsecond contract and provide a supported finding or explicit source-backed disposition. Do not call this covered without evidence.
- **R2 — false export characterization (S01-03):** DATE001 calls `make_date/4`'s spec exported. It is private; actual exports3–19 prove this. Correct the claim and check other visibility statements. Source EDoc/types can support intent without misrepresenting the API.
- **R3 — missing explicit finding scales (S01-03):** audit guide02 requires Scale for every finding. Supply actual scale fields and an explicit scale coverage account; absent service/workspace scales remain absent.

### Row walk and evidence strength

All six opening rows remain accounted for. S01-01 intake is checked against actual source/manifest and remains supported. S01-04 native observations and S01-05 unchanged source/artifact boundary independently reproduce, with formatter/linter and OTP-matrix limits unchanged. S01-02/03 need R1–R3 before complete review acceptance; S01-06 remains open until corrected close/bubble-up, CDC final review and retrospective. Earlier CC dispositions remain preserved at3dc34cf. No acceptance standard changed.

No arc scope/sequencing change is required: this is original date-helper coverage. Slice02 cannot open until this slice is accepted. Five-refinement maximum; iteration01 now issued, zero refinements executed at this review.

## Refinement01 review — candidate6054bad, 2026-10-31

CDC inspected every changed report/close/ledger passage and the complete ten-case runner/output. Independently reran the runner and obtained byte-identical stdout:7 fractional-unit arithmetic mismatches and3 valid controls with stated1e-12 tolerance. R1's original omitted path now has ERL-DATE-005 with contract caveat and legacy/compatibility limits; R2 private visibility and R3 per-finding scales are corrected. Prior native/negative checks remain applicable on unchanged source and their logs retain original authorship/date.

**Verdict: substantive findings supported; return for narrow report-precision correction02 before slice acceptance.** The new scale paragraph says the repository has no command-line executable; `scripts/check_coverage.escript` is a first-party executable explicitly mapped to Arc02. Absence applies to an active date runtime service, not that tool. DATE002 also says README/EDoc document offsets for years<=0; their wording is negative years, while year0 restriction is source/historical intent. State those evidence boundaries accurately. These are metadata corrections, not added code-analysis scope or changed findings. S01-02/03/06 remain pending final corrected report. Iteration01 executed once; correction02 now issued under the same five-refinement cap.

## Final Slice01 review — candidate76eaf7a, 2026-10-31

**Verdict: accepted at slice scope.** CDC read the complete6054bad→76eaf7a report/close/ledger delta, checked actual source/README and mapped coverage-escript existence, and confirmed both precision corrections. No production/probe/dependency inputs changed; retain initial native reproduction and all initial/iteration01 independent behavioral evidence under correction-review retention. Do not relabel those retained runs as new. Source and planning were clean at76eaf7a before this reviewer update; source remainsbcfd94a with no tracked diff.

Current report has5 findings (4Medium,1Low), each with source locations, scale, expected/actual behavior and controls. Original public probes and all10 refinement cases were independently reproduced; native compile/xref/Dialyzer/EUnit210/PropEr4×100/coverage100% succeeded onOTP28. Formatter/linter remained blocked by actual plugin update failures; OTP20–27 and R15B03 remain unverified. No native aggregate alias, full project audit, remedy or release claim is accepted. Source/historical intentional offset rejection is excluded from defect counts.

### Final row walk and bubble-up

- S01-01 done/reproduced: required manifest/readback and current source/assignment identity checked against actual files. Reading receipts alone were not used as semantic proof.
- S01-02 done/reproduced: all5 date APIs and helper scopes map to the report, including the formerly missing fractional-unit path;6 concrete clean checks retained, no defect quota.
- S01-03 done/reproduced:5 finding observations checked against actual code, external primary definitions where needed and independent expected values; private visibility and explicit scales corrected.
- S01-04 done/reproduced: exact attempted native commands/populations/failures and independent runs retained; local OTP28 proof only.
- S01-05 done/reproduced: committed changes limited to allowed planning artifacts; no source delta; all issued prompts and initial/failed/retried evidence preserved.
- S01-06 done/reproduced:6-row CC walks, corrected scope diff, this independent review and CDC retrospective present. Acceptance is of diagnosis/evidence, not defect repair.

Bubble-up: Slice01 delivers its assigned date-contract diagnosis; Slice02 can now open. Arc ordering/scope stays unchanged. Shared helper findings DATE002/005 need cross-references from calendar/interval analysis, avoiding duplicate counts. Known public-spec observation remains **uncleared, owned by Arc02**: `parse_exact/1` atsrc129 declares`calendar:datetime()` yet documented/exercised`parse_exact("20120203T040506.50")` returns second6.5; private`datetime()` atsrc40 permitsfloat while calendar second() is integer0..59. Current source/official calendar definition and existing positive probe support the mismatch; complete type/tooling reconciliation, severity/fix compatibility belongs to Arc02, not a false type-clean claim. Project roadmap/watch item records this explicit carryover.

Two refinements executed under max5, all preserved. `cdc-retrospective.md` records findings and author/reviewer contributions. Arc composition and Operator review remain open; this acceptance does not inherit either gate.
