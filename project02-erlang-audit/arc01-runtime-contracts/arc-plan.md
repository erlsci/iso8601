# Arc01 — runtime contracts

Version1.2 — 2026-10-31. Status: both slices accepted; CDC assembly review-ready, independent composition and Operator gate pending. Stop here.

## Capability

Deliver independently reproduced diagnosis of all15 public runtime functions and shared private helpers against the current documented contract, including meaningful clean checks, source locations, bounded public-API witnesses, and explicit uncertainty. Source stays byte-identical to `bcfd94a`. This enables an informed Operator review and future hardening decision; it does not repair defects or certify the whole audit.

## Slices and composition

1. `slice01-date-contracts`: format/parse/parse_exact/parse_expanded/format_expanded and shared date/week/ordinal/fraction/UTC helpers; production full read plus relevant existing test witnesses. Findings use `ERL-DATE-NNN`.
2. `slice02-calendar-interval-contracts`: add_time/subtract_time/add_days/add_months/add_years/parse_duration/apply_duration/parse_interval/interval_bounds/format_interval and shared helpers; signed/boundary/composition cases. Findings use `ERL-CAL-NNN`. Consumes accepted Slice01 report; detail only when Slice01 closes.

Together these slices cover every public runtime entrypoint and production helper. Test quality, escript, package/tooling/compatibility judgments, old-audit reconciliation and final modernization synthesis belong to Arc02. Existing tests may be read/run as evidence here without representing their full quality audit complete. Each slice fits a full Erlang guide load, source/tests, focused findings and iteration headroom in its own fresh CC context.

## Method and gates

Governing project plan supplies complete audit map, explicit models/authority, artifact-home override and source checkout. Each slice needs concrete boundary/positive/negative probes through public APIs, actual output/expected output, severity reasoning, limitations and at least five concrete clean checks. Follow all17 Erlang chapters while applying old-OTP/EDoc/public-type exceptions from AGENTS. No style modernization is a repair assignment.

CDC independently inspects reports/source and reruns claimed observations; no summary-only acceptance. CC completion is proposed-done. Every slice closes with its ledger walk, bubble-up, CDC verification and retrospective. CDC then assembles a runtime index and arc close in the final slice artifacts/arc root; parent independently reproduces composition over all15 functions and report links. Operator review/merge remains open and Arc02/03 are held. Any unavailable native gates remain explicitly blocked/unrun with re-entry conditions; fresh local probes cannot imply an OTP matrix pass.

## Current status and carryover

Slice01 independently accepted after2 refinements: DATE001–005 (4Medium,1Low). Shared-helper consequences must cross-reference existing IDs in Slice02. The known `parse_exact/1` calendar return-spec versus float-seconds mismatch (src129/40) is uncleared, explicitly assigned to Arc02 public-type/tooling reconciliation; not a claim that runtime parsing lacks fractional seconds. Slice02 is independently accepted at12ca2cd with3High/1Medium; combined9 IDs =3High/5Medium/1Low. Current helper map38 date +14 arithmetic/duration +22 interval =74 forms/70 names, including duration-onlygi and sharedapply_offset/datetime boundaries. Reduced-precision zoned-start contract question remains uncleared for Arc02 beside the public spec mismatch. No arc structural/sequencing change; source staysbcfd94a. Projectv1.2 separately adds later narrowly authorized OTP28/29 CI compatibility Arc03; current arc remains diagnosis-only.

## Version history

- v1.0 — 2026-10-31: source-grounded two-slice runtime diagnosis; no production changes authorized.

- v1.1 — 2026-10-31: Slice01 close surfaced explicit shared-helper cross-references and public-return-spec carryover; status updated, original two-slice runtime scope retained.

- v1.2 — 2026-10-31: Slice02 close completed runtime/helper diagnosis, corrected semanticgi ownership and surfaced an explicit reduced-precision-zone contract carryover. Both slices accepted; parent composition/human gate remain pending, current scope unchanged.
