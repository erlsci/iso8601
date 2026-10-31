# Slice01 — date contracts

Version1.0 — 2026-10-31. Status: assigned to CC; initial assignment, no refinements executed.

## Goal and scope

Diagnose date parsing/formatting and expanded-year behavior through all five public date entrypoints, with source-backed, independently reproducible findings. Read all production source so shared apply_offset/calendar interactions are understood; assigned analysis focuses src/iso8601.erl lines95–156,236–505,544–555 and relevant types. Read all README and both test modules for supported behavior and contrasting cases. Runtime arithmetic/durations/intervals belong to Slice02 except shared date helper consequences. Full test-quality and tooling/OTP audit remains Arc02.

Artifacts: `artifacts/` here, explicitly overriding code-auditing workbench output. Report `artifacts/2026.10.31-audit-results-erlang-date.md`; `artifacts/runlog.md` holds intake, commands/results and self-review. Any focused reproducible probe source belongs here; compiled outputs remain temporary/untracked. Current source baseline `bcfd94a`, isolated branch/checkout and full authority in project plan. No source edits, build-config edits, test repairs, push or merge.

## Binding method and checks

Record observed contract versus inference separately. Inspect full public specs, binary/list input, exact seconds, formatting near rounding boundaries, positive/zero/negative/expanded years, leap/ordinal/week boundaries and UTC offsets. Probe anomalies with positive and negative controls, using monitored workers for potential unbounded work. Do not prescribe a finding from a hypothesis. Exact expected behavior follows README, EDoc/types, specific tests or authoritative primary documentation; unsupported ISO conformance assertions stay questions. Preserve intentionally exported types, EDoc and older OTP compatibility. Concrete guide applications: API-06/07 and DT-16/18 evaluate public specs without confusing Dialyzer with proof; EH-07/10 and FT-02 locate actual input/error boundaries; FP-04/12 and PF-08 check guard/clause/recursion behavior; DC-08 checks documented failure behavior; TE-11 uses actual public APIs; AP-02/06/09/10 scan concrete hazards.

Applicable native gates are read from the repository and attempted on a fresh checkout. Preserve exact failures and dependent blocked gates; fresh erlc/erl/EUnit fallback is separately labelled. No old _build/beams are used. Native coverage thresholds remain unchanged. Finding IDs `ERL-DATE-NNN`, every finding has precise lines, impact, severity reason, expected/actual witness and concrete fix direction; at least five concrete clean checks. No artificial positive-finding quota.

## Author readiness

CDC read all744 production lines, all1069 EUnit lines, all111 property lines, app manifest, README, Make/rebar/CI/lint/coverage script; all17 Erlang chapters plus relevant framework, audit, prompt, PM and verification contracts. Full map appears in project plan before findings. Consequential scope/method/authority resolved; actual conclusions remain evidence-dependent. Reading packet is roughly70k domain-guide tokens plus source/test/contracts, leaving a fresh worker context room for focused work and five-refinement maximum. Exact commands/oracles below in preserved prompt. Initial sandbox denial when creating source branch was recovered through approved escalation; resulting checkout starts clean at baseline.

## Assignment history

- Initial `cc-prompt.md`, issued2026-10-31 by CDC; predecessor none; active. CC owns this slice's allowed evidence/closing-report/ledger updates and isolated generated build outputs during handoff. CDC pauses planning mutation until return.

## Exit

Every ledger row accounted for; report/probes/logs committed proposed-done; CDC reruns evidence and records acceptance or numbered correction; retrospective complete. Bubble up any scope surprises before Slice02 is detailed. Human arc gate remains open.
