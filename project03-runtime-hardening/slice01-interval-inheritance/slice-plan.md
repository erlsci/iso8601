# Slice01 — interval inheritance correctness

Version1.1 — 2026-10-31. Status: narrow refinement01 active. Current assignment `cc-prompt-iteration01.md`; zero refinements executed/max5, initial candidateec665b8 preserved.

## Outcome and scope

Fix CAL003/CAL004 together in the private interval reconstruction path, retaining public shapes, 15 exports/11 types and useful parser/formatter behavior. Correct bounds and semantic format/parse round trips for signed positive years and timezone-inheriting abbreviated endpoints. This is the first repair checkpoint under canonical one-slice collapse; no arc wrapper. Deliver a source candidate on `codex/project03-arc01-intervals` frombcfd94a, meaningful public regressions and README explanation; main integration remains human pending.

Source mutations only `src/iso8601.erl`, `test/iso8601_tests.erl`, `README.md`. No arithmetic/formatter/global timestamp parser repair, iodata repair, build/CI/app/dependency or guidance change. In particular minute-precision zoned START remains deferred, negative-year interval endpoints still reject, intentional basic-date-start abbreviation and month-day+time abbreviation badarg remain. Other seven audit findings must be replayed and honestly left open. Durable outputs: `artifacts/runlog.md`, red/green/audit-replay native logs or one directly useful transport runner if necessary, `closing-report.md`; no duplicate audit reports or unrelated tooling product.

## Binding behavior and source-grounded route

Atbcfd94a `resolve_end/2`638–642 relies on unsigned-only `year_prefix/1`650–662; `extract_year/1`715–716 and `extract_year_month/1`718–719 take raw fixed4/7 characters. Use a complete signed-positive year token for both classification and inherited year/month prefixes. The original unsigned4-digit path and basic-abbreviated rejection must remain. A complete signed end is parsed independently by the same `parse_endpoint/1`; it must not be mistaken for a fragment. Keep the existing endpoint parser's rejection/format limits, no new negative-year interval contract.

At664–680 fragments are rebuilt from raw start text but omit its zone. Preserve raw local date/year/month when building the end, before UTC normalization; extract any start zone only from its time portion after T, never a signed year/date hyphen. An explicit end Z/+/- zone wins; a fully specified endpoint stays independently parsed, not implicitly zoned. For supported abbreviated day/month-day/day+time/time-only/Ttime endings lacking their own zone, inherit the raw start zone. Missing time defaults to midnight. Before appending an inherited zone only, complete hour/minute time to seconds (HH→HH:00:00, HH:MM→HH:MM:00, basicHHMM→HHMM00, digit guards required); existing seconds/fractional forms remain literal. This local normalization avoids the known underlying minute+zone parser limitation without broadening the standalone parser. No zone start means preserve the old unzoned route. Recommended private helper names/mechanics are local discretion; behavior/ordering are binding.

Use existing lists/string operations and pattern clauses, a constant number of bounded concatenations/scans over the input. Avoid numeric year reformatting that loses original token precision, zone detection before T, UTC-normalized date inheritance, blanket catches, dynamic atoms, public test hooks or new syntax/dependencies. Pure parsing has no service/process/resource lifecycle to invent. Reusing the existing decimal parser retains float-fraction behavior without sending already-float datetimes through CAL002 or formatting away precision.

## Acceptance examples and oracles

Use exact EUnit public results, not only shape/round-trip equality. Calendar arithmetic/explicit offset controls independently determine expected UTC values. Tests must first fail unchanged productionbcfd94a; commit/preserve red evidence before production changes, then pass after repair. Preserve matching passing controls so unconditional offset/year rewrites fail.

- `+2007-11-13/15` →end2007-11-15 midnight; `+0002007-02-15/03-14` →end2007-03-14 midnight; signed full end `2007-12-14/+10000-12-15` →endyear10000, not badarg. Signed year/day+time and formatter-produced start paths must also retain full year.
- `2007-12-14T13:30:00+02:00/15:30:00` →UTCbounds11:30/13:30. Negative offset -02 gives end17:30. Explicit end Z gives15:30; explicit -03 gives18:30. Fully specified end without zone remains UTC15:30.
- `2007-01-01T00:30:00+02:00/00:45:00` →end2006-12-31 22:45; combined `+10000-01-01T00:30:00+02:00/T00:45:00` →end9999-12-31 22:45. These reject deriving end date from normalized start.
- End `.125` retains exact fraction with inherited +02; minute end15:30 becomesUTC13:30 via local :00 completion. T-prefixed hour and T-prefixed basic HHMM ending support stays; a bare four-digit token remains a full year endpoint; explicit minute+zone rejection is not silently changed.
- Day-only15 or month-day12-15 from local2007-12-14 with+02 →localDec15 midnight →UTCDec14 22:00; day+time15T17:00 with+02 →UTCNov15 15:00. Include fractional-hour/minute literal controls if exercising that completion path, preserving known DATE005 rounding rather than fixing it here.
- Existing unsigned/unzoned/four interval forms/both separators/signed durations/zero durations remain valid; malformed multipleT/empty sides/day9, negative full endpoints, basic-start abbreviation and month-day+time abbreviation remain badarg. Year0/nonzero-zone restrictions stay.

Native gates in the prompt plus all-nine audit-probe applicability establish candidate validation; coverage100 is execution evidence only. Parent independently checks the complete candidate after CDC acceptance, including its own controls. Operator gate remains open even if every source test passes.

## Author readiness and guideline application

CDC inspected actual full production/tests during Project02 and refreshed all interval helpers561–744, timestamp year/time/offset dispatch236–390, interval/expanded regressions and README/Make/rebar/CI. Baseline is unchanged; accepted findings and prior source-cited expected witnesses apply as red expectations, not new candidate validation. Fully reloaded anti-pattern11 first and relevant API02, error03, data04, functions05, testing15, tooling17 chapters before settling this design; prompt-authoring07 full and PM README/top-down/collapse plus testing discipline/gates refreshed. Other earlier full guide readings are context, not new receipts.

API-01/02 SHOULD: preserve small explicit exports/private helpers. EH-07/10 SHOULD and AP-06 AVOID: use existing boundary parser rejection, no fallback catch. DT-10/11 SHOULD: current parser consumes small flat lists, so retain its representation with bounded concatenations rather than unrelated binary rewrite; no growing-left loop. FP-01/04/12 SHOULD: specific guarded digit/time clauses before generic unchanged path. TE-01/02/11 SHOULD: exact public EUnit regression results and independent offset arithmetic; TE-10 CONSIDER yields to explicit100% repository gate without vacuous assertions. TL-01/04/05/07/09 SHOULD: use actual native static/test/format/lint commands and disclose blocked providers. Repo older-OTP/EDoc/type constraints override newer-guide recommendations. Source-grounded route and discriminating day-boundary/minute controls resolve behavior; no investigation-only packet or unresolved design decision is assigned.

## Assignment history

- `cc-prompt.md` — issued2026-10-31, predecessor none, initial implementation; returned as sourceec665b8/planning3908b5a, superseded by refinement01. CC owns allowed source and slice artifacts/ledger/close during handoff; CDC owns other Project03 plans/reviewer records, parent owns existing Project02/README. Five numbered refinements maximum, no next increment before human gate.

- `cc-prompt-iteration01.md` — issued2026-10-31, predecessor `cc-prompt.md`; actual formatter-derived regression, README numbering and source-backed evidence/read receipts; active. Production byte-identical, no scope change.

## Version history

- v1.0 — 2026-10-31: initial coherent collapsed repair checkpoint.
- v1.1 — 2026-10-31: CDC independently supported production/native/replay behavior; narrow test/doc/evidence refinement01 required before acceptance. Full original runner population corrects inherited prompt shorthand38 to42; all147 observations retained.
