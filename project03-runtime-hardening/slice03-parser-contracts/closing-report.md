# Slice03 CC proposed-done report

## Identity and pins

This report closes the CC implementation pass for the original `cc-prompt.md` at planning commit `e5652e53e74c413f7d0ef8b566010d195285857a`, under the existing Slice03 plan. Source began at `e332032561142bff6c620cd1e7b4a5bfe9929b0b` (tree `72efe0dc781ed31cdac78872670465b92940da80`) and is now committed on `codex/project03-arc03-parsers` as `22b92d10b496ab541d3b8c369c4a081309bf0134` (tree `643ed3352b66b1d19035b6fd61b7e975d869d985`). The source worktree is clean. The commit changes only `README.md`, `src/iso8601.erl`, and `test/iso8601_tests.erl`; it preserves the required adjacent coauthor trailers. No push, PR, merge, release, or publication occurred.

The planning packet is committed on the orphan `planning` branch. Its exact commit and tree are in the follow-up metadata entry at the end of `artifacts/runlog.md` and are supplied with this report to CDC.

## Delivered behavior and evidence

The implementation repairs ERL-DATE-001, ERL-DATE-002, and ERL-DATE-004. All four public iodata parser entrypoints normalize byte iodata before dispatch. Raw Gregorian month/day and ISO week/weekday fields are validated before offset normalization; week 53 is accepted exactly for Thursday-start years or leap Wednesday-start years. The existing year-zero nonzero-offset restriction and duration-string contracts remain. README/EDoc explain the boundaries and constraints.

The exact test-first baseline run used unchanged production commit/tree `e332032` / `72efe0d`, fresh OTP28 beams, and literal `badarg` assertions. It exited with 87 failures and 334 passes: 45 DATE001, 26 DATE002, 13 valid-iodata acceptance, and 3 invalid-interval-input exception-reason differences. Those last three `string:split/3` `badmatch` reasons are out-of-contract baseline details; the strengthened exact `badarg` oracle is preserved and passes on the candidate. The earlier 86- and 87-failure attempts, the weakened-oracle correction, and the corrected exact run are all retained in `artifacts/` and explained in the runlog.

All eight required local OTP28 gates passed sequentially: compile, xref, Dialyzer, EUnit (430), PropEr (4/4), configured coverage (module and total 100%), formatter check, and lint. Earlier Dialyzer and formatting failures and their resolution remain recorded. All 147 immutable observations were replayed from fresh candidate beams: 31 date, 10 fractional-unit, 42 calendar/interval, and 64 iteration01 observations. All matched their recorded expectations; all ten fractional comparisons met tolerance `1e-12`. Replay covers the six prior fixes DATE003, DATE005, CAL001–004 as well as the three current repairs. Exact commands, outputs, hashes, and denominators are indexed in the runlog and artifacts.

Declaration comparison found no change to the 15 public functions, 11 exported types, all 16 baseline type declarations, or 15 public specs. App version remains 1.4.0. Added tests preserve the original suite and include four-boundary nested/improper/empty/malformed iodata, literal raw-date/week controls, interval syntax and shapes, and fractional exact-unit → duration → formatter composition.

## Ledger walk

| Row | CC disposition | Evidence / remaining owner |
|---|---|---|
| S03-01 | CC-proposed, attested | Complete bounded intake, source-cited corrections, and explicit CDC clearance are recorded in `artifacts/runlog.md`; CDC's review remains part of S03-08. |
| S03-02 | CC-proposed, attested | Exact red, 430-test green, and literal date/calendar replay controls are recorded. |
| S03-03 | CC-proposed, attested | Exact red, 430-test green, and proleptic date replay controls are recorded. |
| S03-04 | CC-proposed, attested | Exact red, 430-test green, four-entrypoint valid/invalid iodata controls are recorded. |
| S03-05 | CC-proposed, attested | Three-path diff/declaration audit and 147-observation replay are recorded; CDC independent review remains open. |
| S03-06 | CC-proposed, attested | Eight exact local gate receipts are retained; hosted matrix is not claimed. |
| S03-07 | Open | Parent owns actual reviewed final-head OTP20–29 hosted validation. |
| S03-08 | Open | CDC owns independent reproduction, full row walk, and retrospective. |

## Finding disposition and boundary

All nine stable finding IDs are accounted for. DATE001, DATE002, and DATE004 are repaired in this slice. DATE003, DATE005, CAL001, CAL002, CAL003, and CAL004 are retained as prior passing repairs in the fresh replay. No finding was silently dropped and no new issue was inferred from the three out-of-contract interval exception-reason differences. A reduced-precision zoned-start case still returns `badarg` and remains the prior non-finding candidate disposition.

No plan or scope bubble-up was identified: the implementation and evidence address the initial assignment and current slice criteria within the allowlist. CC self-review is not independent verification. This report is therefore proposed-done only; CDC verification, parent hosted CI, Operator acceptance, and later project gates remain distinct and outstanding. The local OTP28 results do not establish the hosted OTP20–29 matrix, `make check`, or release readiness.

## Artifact index

- `artifacts/runlog.md`: intake/readback and corrections, clearance chronology, test-first baseline, implementation, native gates, replay, declaration audit, findings, and close receipts.
- `artifacts/baseline-red-*.log`: retained baseline attempts, exact corrected oracle, pins, toolchain, commands, outputs, and exits.
- `artifacts/native-final-01-compile.log` through `artifacts/native-final-08-lint.log`: final required local gates.
- `artifacts/native-*-attempt*.log` and `artifacts/format-write-attempt*.log`: retained earlier failures/retries.
- `artifacts/replay-*.log`: immutable runner provenance, candidate beam and source hashes, compile receipts, replay outputs, and the one corrected temporary-path attempt.
