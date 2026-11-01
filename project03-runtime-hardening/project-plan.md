# Project03 — runtime hardening

- project: `project03-runtime-hardening`
- status: first interval slice CDC accepted; parent composition and Operator checkpoint pending; remaining roadmap held
- depends-on: accepted Project02 Arc01 diagnosis at planningcfbf539; sourcebcfd94a v1.4.0
- related: `project02-erlang-audit`; historical `project01-v1.4-maintenance`
- blocks: claims that the nine accepted runtime findings are repaired
- version: 1.1 — 2026-10-31

## Outcome, authority and endpoint

Repair all nine accepted Project02 Arc01 findings while preserving the 15 public exports, 11 intentionally exported types, return shapes and broad OTP compatibility. Add meaningful regression tests that fail unchanged source and pass repaired source, source-grounded documentation, independently reproduced validation and a finding-by-finding applicability handoff. Original audit reports remain immutable dated evidence, not rewritten into green reports.

Direct Operator decision: “I've looked at the findings — I want us to fix them all :-D How do we get started?” Parent relayed continued Autonomous Mode with CDC GPT-6.1 Sol High and fresh CC GPT-6 Luna High, and a stop after ONE repair arc/increment for human review and merge discussion. The first coherent interval repair fits one slice; canonical one-slice collapse puts its documents directly in `slice01-interval-inheritance/`, with no ceremonial arc wrapper. This slice is the first repair-arc checkpoint for the authorization; the branch's arc01 spelling does not require extra arc documents. Parent independently gates its final composition and the Operator retains the review/merge/next-increment decision. No push, PR, merge, release, publication, external issue/message or second repair increment is authorized now.

Project02 remains incomplete: its Arc02 tests/escript/tooling/type/historical/synthesis audit and Arc03 OTP28/29 CI plus necessary compatibility work stay open, separately owned. They are not cancelled or silently absorbed into runtime repair. Known parse_exact public return-spec and reduced-precision zoned-start questions remain Project02 obligations. This project does not claim a complete audit, supported OTP matrix or release certification.

## Full remediation roadmap

| Increment | Capability, findings and destination | Dependency / current state |
|---|---|---|
| First repair checkpoint — collapsed Slice01 | Correct interval signed positive-year classification/inheritance (ERL-CAL-003) and omitted-zone inheritance (ERL-CAL-004); source module, public regression tests and README | CDC accepted source7e93c5d; parent/human checkpoint pending, one coherent slice |
| Later arithmetic/precision arc | Bounded arithmetic floor behavior CAL001, float-anchor composition CAL002, formatter carry DATE003 and exact fractional-unit retention DATE005 | Plan in detail after first checkpoint. Preserve parse/1 legacy rounding; exact APIs retain fractions. A bounded badarg floor policy is the likely design but needs actual source/contract grounding before assignment. Four findings remain open. |
| Later parser-contract arc | Validate week boundaries DATE001 and non-positive-year dates DATE002; make declared iodata inputs work consistently DATE004 | Plan in detail after prior repairs. Keep intended negative-year/nonzero-offset restrictions and stable public error contracts; support declared iodata rather than narrowing it without authorization. Three findings remain open. |

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
