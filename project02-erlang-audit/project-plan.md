# Project02 — Erlang audit

- project: `project02-erlang-audit`
- status: active; Arc01 authorized, Arc02 roadmap only
- depends-on: historical preservation completed at planning `213bfc7`; source baseline `bcfd94a`
- blocks: any claim of a complete current audit or a later hardening plan
- related: `project01-v1.4-maintenance`; iso8601 v1.4.0
- version: 1.0 — 2026-10-31

## Outcome and boundaries

Deliver an evidence-based diagnosis of all first-party Erlang and its public, test, build and compatibility contracts. Findings have stable IDs, severity, precise source locations, expected/observed public behavior, concrete fix directions and independent reproduction. Final project delivery includes an Erlang report, complete audit index, historical-claim reconciliation, and evidence-backed modernization synthesis. No production, test or build-configuration repair belongs to this project. This is an audit, not an implementation or release certification.

The Operator authorized historical migration first, the standard orphan planning layout, then a new indexed project and Autonomous Mode for **ONE ARC ONLY**. Arc01 stops review-ready for human review and merge discussion. Arc02 cannot start until separately authorized. A diagnosis-only arc may have no source diff; its useful delivery is committed planning evidence. Source fixes need a later authorized hardening plan. No push, PR, merge, publication, release, issue creation or external messages are authorized.

## Audit architecture and complete map

Source baseline is `bcfd94a`, the guidance cleanup above `c594869` (v1.4.0). The library is one pure synchronous module; callers own processes and failure handling. No services, supervision tree, workspace members, FFI, network or persistence subsystem exists. README/API/EDoc are current context; no standalone current architecture/design document is tracked. Historical interval design is provenance, not a current assignment. Record this discovery gap in audit summaries and assess its consequence proportionately.

| Tracked surface | Role and review destination |
|---|---|
| `src/iso8601.erl` | All 744 lines; 15 public functions and 11 intentionally exported types; Arc01 runtime and Arc02 language/tooling reconciliation |
| `src/iso8601.app.src` | Library app/version/dependencies/package metadata; Arc02, context for Arc01 |
| `test/iso8601_tests.erl` | EUnit including parser, arithmetic, interval, expanded-year cases and capture helper; runtime witnesses Arc01, full test quality Arc02 |
| `test/prop_iso8601.erl` | Four properties and duration helper; runtime witnesses Arc01, oracle/population quality Arc02 |
| `scripts/check_coverage.escript` | First-party Erlang executable; all 91 lines and exit/coverage contracts Arc02 |
| `README.md` | Public usage, supported representations/limits, compatibility badge; both arcs |
| `rebar.config`, `elvis.config`, `Makefile`, `.github/workflows/ci.yml` | Tool/profile/gate/platform contracts; context Arc01, diagnosis Arc02 |
| `AGENTS.md`, `CLAUDE.md` | Governing instructions and symlink; both arcs |
| `.gitignore`, `LICENSE`, `priv/images/logo.png`, `priv/images/logo-large.png` | Repository/package boundaries; no executable language audit of license/images |

There are 16 tracked entries including the symlink. Erlang is audited including its escript and manifest. Make and YAML are detected contract/configuration languages with no matching installed language skill; inspect their operational promises without claiming a separate language audit. Markdown is contract/provenance text, PNGs are static assets. Generated `_build`, `.eunit`, beams, docs/logs, dependency caches, `.git`, nested worktrees and ignored workbench scratch are excluded from source populations. Archived history remains preserved and its acceptance rows stay open.

## Roadmap

1. **Arc01 — runtime contracts:** independently checked diagnosis of date parsing/formatting/expanded representations and arithmetic/duration/interval composition. First usable increment: public-API date-contract findings and reproducible probes. Delivery is the owning slices' artifacts and an arc close packet on `planning`.
2. **Arc02 — validation and audit synthesis:** full test/escript/tooling/OTP/package-contract audit; reconcile historical findings against actual source/authoritative OTP documentation; assemble final one-language report, index and modernization synthesis with compatibility classifications. Consumes accepted Arc01 findings at their pins; roadmap only, no detailed arc/slice directories yet.

## Workflow, models and ownership

Operator-selected Autonomous Mode uses two actual contributor contexts: CDC OpenAI GPT-6.1 Sol, High (planning, source-grounded review and independent reproduction); CC OpenAI GPT-6 Luna, High (bounded diagnosis/evidence assembly). These are explicit Operator choices relayed in the orchestration assignment; no fallback is authorized. Parent orchestration context retains integrated accountability and independently reviews arc composition. Model names alone do not establish independence.

Source audit checkout: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit`, branch `codex/project02-arc01-audit`, baseline `bcfd94a`. Planning: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/planning`, orphan branch `planning`. CDC owns planning except explicit sequential CC handoffs. CC owns generated outputs in the isolated source checkout while executing; no tracked source mutation. No overlapping installs/builds/scanners. Each slice uses reviewer-authored `cdc-verification.md` plus CDC `cdc-retrospective.md`; arc closure assembled by CDC remains subject to parent independent composition review and Operator gate.

## Evidence and validation contract

Audit date captured with `date +%Y.%m.%d`: `2026.10.31`. Per-slice durable artifacts override code-auditing's `workbench/` default under the repository/Operator standard. Keep distinct failed, retried, CC and CDC observations in existing run logs; no stale May build output counts as a current baseline. Exact commands, exit statuses, actual selected populations and limits are required. Source expected values come from documented contracts or independently derived calendar cases, not another call to the same implementation. Hang hypotheses use monitored workers with a timeout and kill/cleanup.

Actual preflight observed local rebar3 3.27.0 / OTP 28, outside configured OTP20–27. Revalidate in execution. CI runs compile/xref/Dialyzer on20–27 and tests/PropEr/coverage on21–27; R15B03 is intended historical floor, not demonstrated here. Rebar check/CI requires100% coverage; Make script requires95%; neither is lowered. Attempt applicable native gates in a fresh isolated checkout; preserve failures. If dependency loading is unavailable, disclose blocked gates and use fresh local erlc/erl public-API probes as bounded evidence, never a replacement claim of full CI success.

## Historical migration independent check

CDC independently compared all12 archived files against source bytes, SHA256 and source/archive nanosecond mtimes; all match. All30 historical acceptance rows across9 ledgers remain OPEN; exactly2 arcs/6 slices. All81 authored relative links resolve; all14 task/mainline patch-ID pairs match; planning has one parentless root. History remains historical and is not accepted retroactively. Migration baseline `213bfc7`; these checks were read-only and no historical file was changed.

## Definition of done

See sibling `ledger.md`. Project completion requires both arcs, complete mapped scope, independent evidence and explicit Operator acceptance. Arc01 findings are not a claim that the whole project or prior release is accepted.

## Version history

- v1.0 — 2026-10-31: CDC opened two-arc diagnosis roadmap after byte-preserved historical migration; only Arc01 execution authorized.
