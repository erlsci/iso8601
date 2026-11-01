# iso8601 planning index

## Project01 — v1.4 maintenance history

[project01-v1.4-maintenance/project-plan.md](project01-v1.4-maintenance/project-plan.md) is a best-effort historical reconstruction from the preserved workbench records and first-parent Git history. The project, arcs, and slices are marked `historical-reconstructed`; all historical acceptance rows remain open.

## Project02 — Erlang audit

[project02-erlang-audit/project-plan.md](project02-erlang-audit/project-plan.md) opens the current diagnosis-only audit after historical preservation. Arc01 runtime contracts are independently verified and Operator-reviewed. Arc02 diagnostic synthesis and Arc03 OTP28/29 CI work remain open and unexecuted. Historical acceptance remains open.

## Project03 — runtime hardening

[project03-runtime-hardening/project-plan.md](project03-runtime-hardening/project-plan.md) tracks the Operator-authorized repairs for all nine accepted runtime findings. The first interval inheritance checkpoint is independently verified and awaiting Operator review/merge; seven remaining findings are grouped into arithmetic/precision and parser contracts. Existing Sol High CDC / Luna High CC roles and the stop after every repair arc remain in force.

## Migration checks and limits

For commit `f82f7e9`, all 12 archived originals were compared with their source SHA-256 and nanosecond modification times; authored relative links resolved; the six-slice layout and orphan history were checked; all project, arc, and slice carryover rows remained open; and 14 task-branch/mainline patch pairs were confirmed by stable patch ID. `git show --check` passed. The source worktree remains at `bcfd94a` with no source edits from this migration. This planning-only commit did not rerun Erlang checks or CI, and the archived behavior/acceptance claims still need independent evidence. The bounded AGENTS cleanup self-check is separate from historical acceptance.
