# Project01 — v1.4 maintenance

- **Project:** `project01-v1.4-maintenance`
- **Status:** `historical-reconstructed`
- **Depends-on:** none recorded
- **Blocks:** none recorded
- **Related:** iso8601 source history through `bcfd94a`; preserved `workbench/` material, indexed in [the provenance manifest](artifacts/provenance-manifest.md)

## Purpose and historical scope

This record reconstructs a body of work associated with the 1.4.0 release from surviving workbench documents and first-parent Git history. It covers audit and coverage work, audit repairs and capture-output hardening, interval support, and expanded-year support. It does not assert that the old prompts were issued exactly as written, that every described criterion was met, or that any historical review or release gate was accepted.

The current source tree reports version 1.4.0, but this reconstruction does not certify the release or its acceptance. The historical documents contain some instructions that refer to older version values, issues, and now-removed `priv/ai` paths; those are retained verbatim. See the project ledgers and slice plans for the evidence boundary.

## Boundaries

Included are only the six document-backed historical slices listed below and the original workbench files mapped to them. No missing prompts, closing reports, verification records, reviews, retrospectives, or acceptance decisions have been recreated. No present-day source audit or implementation assignment is opened here.

## Arc roadmap

1. [Arc01 — audit and coverage](arc01-audit-and-coverage/arc-plan.md): historical audit/coverage work and related repairs. It has three document-backed slices: audit and coverage; audit repairs; output capture.
2. [Arc02 — intervals and expanded years](arc02-intervals-and-expanded-years/arc-plan.md): historical interval and expanded-year feature work. It has three document-backed slices: time intervals; positive expanded years; negative expanded years.

The roadmap order follows the surviving first-parent sequence: audit/coverage and repair work precede interval work, which precedes expanded-year work. This is a historical organization, not evidence of formal arc gates.

## Definition of done and open acceptance

The original project outcome cannot be reconstructed with enough precision to assert historical acceptance. The project-level ledger therefore records the recoverable claims as **open**. Project completion would require a fresh evidence review of the source state and applicable release/acceptance obligations; this migration supplies neither.

## Workflow and authority

This is a historical reconstruction prepared by CC under the Operator's explicit migration authorization. It is not a retrospective substitute for the original workflow and no independent review or Operator acceptance is recorded. The source cleanup and commit for the current `AGENTS.md` are a separate bounded self-check.

## Version history

- **v1.0 — 2026-10-31:** reconstructed the historical project from 12 preserved workbench files and first-parent history. This plan does not backdate itself or close historical work.
