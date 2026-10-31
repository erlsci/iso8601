# iso8601 planning index

## Project01 — v1.4 maintenance history

[project01-v1.4-maintenance/project-plan.md](project01-v1.4-maintenance/project-plan.md) is a best-effort historical reconstruction from the preserved workbench records and first-parent Git history. The project, arcs, and slices are marked `historical-reconstructed`; all historical acceptance rows remain open.

## Project02 — Erlang audit

[project02-erlang-audit/project-plan.md](project02-erlang-audit/project-plan.md) opens the current diagnosis-only audit after historical preservation. Autonomous execution is authorized for Arc01 runtime contracts only; stop for Operator review before any next arc. Historical acceptance remains open.

## Migration checks and limits

For commit `f82f7e9`, all 12 archived originals were compared with their source SHA-256 and nanosecond modification times; authored relative links resolved; the six-slice layout and orphan history were checked; all project, arc, and slice carryover rows remained open; and 14 task-branch/mainline patch pairs were confirmed by stable patch ID. `git show --check` passed. The source worktree remains at `bcfd94a` with no source edits from this migration. This planning-only commit did not rerun Erlang checks or CI, and the archived behavior/acceptance claims still need independent evidence. The bounded AGENTS cleanup self-check is separate from historical acceptance.
