# Slice02 — calendar and interval contracts

Version 1.0 — 2026-10-31. Status: assigned, initial pass. This is the final slice of authorized Arc01; stop at review-ready arc/human gate.

## Outcome and scope

Deliver diagnosis of the remaining ten public APIs: add_time/4, subtract_time/4, add_days/2, add_months/2, add_years/2, parse_duration/1, apply_duration/2, parse_interval/1, interval_bounds/1, format_interval/1; inspect all shared production helpers in context. Source stays byte-identical to bcfd94a in dedicated audit worktree. Source repair, test/config edits, whole test-quality/escript/OTP audit, old-audit reconciliation and modernization synthesis are excluded; Arc02 owns those mapped obligations. No active service exists in library runtime; first-party coverage escript exists and is Arc02-deferred.

Artifacts: this slice's `artifacts/`, overriding audit guide workbench default. Report `2026.10.31-audit-results-erlang-calendar.md`, reproducible bounded public probes and runlog; runtime `2026.10.31-audit-index.md` composes all15 APIs across accepted date report and this report, explicitly partial to full project. No separate final modernization synthesis yet.

## Method and engineering decisions

Binding: use actual documented/source/history contract and public calls, preserve stable API/type/error semantics and old-OTP/EDoc choices. Historical interval design/hardening are intent evidence, not active assignments or accepted source facts. Current tests explicitly lock basic-start inheritance and month-day+time abbreviation as badarg; do not invent those as new unsupported-feature findings. Signed durations and clamp-then-component arithmetic are deliberate; month-clamping is not invertible, so do not require duration/end to invert start/duration without a governing promise. Documented duration fractional truncation is deliberate. Keep negative-year offset intent distinct from invalid-date bypass.

Recommended inspection: trace timestamp dispatch, Gregorian and month/year boundaries, finite day-clamping termination, fractional datetime composition, duration capture/sign/component order, all four interval forms, both separators, fragment classification/inheritance, UTC/expanded-year prefixes and duration output. Validate parsed/derived dates via independent calendar arithmetic where representable. For a possible infinite clamp, combine monitored finite worker probes with the recursion invariant and installed calendar valid_date behavior; timeout alone is insufficient proof.

Guide applications: FP-04/08/12 and PF-08 test guards/recursion/termination; EH-10 and FT-02 reason about external duration/date boundaries; DT-16/API-07 distinguish public type claims from actual data; TE-11 exercises public API composition; DC-08 checks error intent; AP-02/06/09/10 scans actual hazards. Preserve guide strength and compatibility exceptions. Shared DATE002/005 helper consequences are cross-referenced; same root cause is not counted twice. Public spec-only quality observations may carry explicitly to Arc02 without a false type-clean conclusion.

## Validation and review

Unchanged source/dependencies/native configuration allow retention of Slice01's independently reproduced OTP28 compile/xref/Dialyzer/EUnit210/PropEr4×100/coverage100% evidence. CC records actual reuse/pins and runs affected focused probes against the known source beam; CDC decides applicability. Re-run a gate only if inputs changed, proof is uncertain or a new concern requires it. Retain formatter/linter plugin-fetch failures and untested OTP20–27/R15B03 limits. No aggregate alias/CI matrix pass claimed. No parallel rebar invocations or unknown-output cleanup.

Each finding uses stable `ERL-CAL-NNN`, severity reason, explicit scale/source lines, expected/actual public witness, meaningful positive/negative controls and fix direction. At least5 concrete clean checks; no defect quota. Whole report/map all15-runtime index with all explicit production helper forms assigned to logical units and close need independent CDC reproduction. Human arc gate stays open.

## Author readiness and assignment

CDC fully inspected all production/tests/contract files, all17 Erlang guides and relevant framework/audit contracts before this assignment; refreshed historical interval intent in full. Independently prepared bounded before-year0, fractional-anchor and expanded-endpoint cases in a separate temporary beam checkout. These are hypotheses and discriminating methods, not prescribed findings. Binding behavior/ownership resolved; evidence-dependent findings remain open. A fresh LunaHigh CC context has packet headroom including full guide/source load.

Assignment history: initial `cc-prompt.md`, issued2026-10-31; predecessor none; active. CC owns only this slice's artifacts/ledger/closing-report and isolated generated outputs during handoff; CDC holds planning writes. Maximum5 refinement passes. No subsequent arc execution authorized.
