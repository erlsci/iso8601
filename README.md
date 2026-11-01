# iso8601 planning index

## Project01 — v1.4 maintenance history

[project01-v1.4-maintenance/project-plan.md](project01-v1.4-maintenance/project-plan.md) is a best-effort historical reconstruction from the preserved workbench records and first-parent Git history. The project, arcs, and slices are marked `historical-reconstructed`; all historical acceptance rows remain open.

## Project02 — Erlang audit

[project02-erlang-audit/project-plan.md](project02-erlang-audit/project-plan.md) opens the current diagnosis-only audit after historical preservation. Arc01 runtime contracts are independently verified and Operator-reviewed. Arc03 OTP28/29 CI is merged through [PR#77](https://github.com/erlsci/iso8601/pull/77); all ten OTP20–29 jobs pass on both the reviewed PR and merged main3a1cdfd. Arc02 diagnostic synthesis remains open. Historical acceptance remains open.

## Project03 — runtime hardening

[project03-runtime-hardening/project-plan.md](project03-runtime-hardening/project-plan.md) tracks the Operator-authorized repairs for all nine accepted runtime findings. The interval inheritance checkpoint is merged via PR#76; the four-finding arithmetic/precision checkpoint is merged via [PR#78](https://github.com/erlsci/iso8601/pull/78), with all ten OTP20–29 jobs passing on both its PR and merged main e332032. The final three parser findings are repaired and merged through [PR#79](https://github.com/erlsci/iso8601/pull/79) into main10a182b; all ten OTP20–29 jobs pass on both its reviewed PR and merged main. All nine runtime findings are repaired. Final project acceptance and the wider Project02 audit remain open. Existing Sol High CDC / Luna High CC roles and the stop after every repair arc remain in force. The Operator selected a separate 1.4.1 release-preparation PR only after all repairs have landed; the source version remains 1.4.0.

## Migration checks and limits

For commit `f82f7e9`, all 12 archived originals were compared with their source SHA-256 and nanosecond modification times; authored relative links resolved; the six-slice layout and orphan history were checked; all project, arc, and slice carryover rows remained open; and 14 task-branch/mainline patch pairs were confirmed by stable patch ID. `git show --check` passed. The source worktree remains at `bcfd94a` with no source edits from this migration. This planning-only commit did not rerun Erlang checks or CI, and the archived behavior/acceptance claims still need independent evidence. The bounded AGENTS cleanup self-check is separate from historical acceptance.
