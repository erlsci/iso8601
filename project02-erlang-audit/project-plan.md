# Project02 — Erlang audit

- project: `project02-erlang-audit`
- status: Arc01 review-ready after slice acceptance; independent composition/Operator gate pending; Arc02/Arc03 roadmap only
- depends-on: historical preservation completed at planning `213bfc7`; source baseline `bcfd94a`
- blocks: any claim of a complete current audit or a later hardening plan
- related: `project01-v1.4-maintenance`; iso8601 v1.4.0
- version: 1.3 — 2026-10-31

## Outcome and boundaries

Deliver an evidence-based diagnosis of all first-party Erlang and its public, test, build and compatibility contracts. Findings have stable IDs, severity, precise source locations, expected/observed public behavior, concrete fix directions and independent reproduction. Final project delivery includes an Erlang report, complete audit index, historical-claim reconciliation, and evidence-backed modernization synthesis. Arc01/02 remain diagnosis-only. A narrow later implementation exception for OTP28/29 CI additions and compatibility fixes necessary for them was directly requested by the Operator on2026-10-31; Arc03 owns it. Other runtime remedies still require a later authorized hardening decision. This project does not certify a release.

The Operator authorized historical migration first, the standard orphan planning layout, then a new indexed project and Autonomous Mode for **ONE ARC ONLY**. Arc01 stops review-ready for human review and merge discussion. Arc02/03 cannot start before the Arc01 checkpoint and subsequent execution decision. A diagnosis-only arc may have no source diff; its useful delivery is committed planning evidence. Source fixes outside the narrow later OTP28/29 compatibility exception need a later authorized hardening plan. No push, PR, merge, publication, release, issue creation or external messages are authorized.

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
2. **Arc02 — validation and audit synthesis:** full test/escript/tooling/OTP/package-contract and public-spec reconciliation audit; reconcile historical findings against actual source/authoritative OTP documentation; assemble final one-language report, index and modernization synthesis with compatibility classifications. Consumes accepted Arc01 findings at their pins; roadmap only, no detailed arc/slice directories yet.
3. **Arc03 — OTP28/29 CI compatibility:** add OTP28 and29 to the CI support contract and make only compatibility fixes necessary for those additions. Preserve existing OTP20–27 support and older-floor discipline unless a reproduced blocker requires an Operator decision; do not drop versions by convenience. Validate actual selected toolchain pins/matrix and record failures. If necessary compatibility fixes change the source pin, reproduce affected audit contracts/findings and reconcile final report applicability; preserve original bcfd94a/date receipts without presenting them as acceptance of changed source. Roadmap only; no detailed arc/slice directories, CI/source edits or execution before current Arc01 checkpoint.

## Dated scope amendment — 2026-10-31

Direct Operator steering: “we're going to need to bump CI to support OTP28 and29 (we might need to provide fixes for those additions, too ...)”. This authorizes planning the narrow Arc03 implementation exception; the current one-arc execution endpoint remains intact. Current `.github/workflows/ci.yml` still configures20–27; neither28 nor29 has been added or validated as a project matrix job. Runtime findings unrelated to the additions remain diagnosis-only.

Parent read primary documentation on2026-10-31: official [OTP29 downloads](https://www.erlang.org/downloads/29) points to OTP-29.1.2 and official `erlang:29` Docker; [OTP28 downloads](https://www.erlang.org/downloads/28) points to OTP-28.5.0.8 and `erlang:28`. [setup-beam README](https://github.com/erlef/setup-beam/blob/main/README.md) lists Ubuntu22.04 with OTP24.2–29. [rebar3 compatibility table](https://github.com/erlang/rebar3) lists3.28.0 forOTP27–29 and3.25.1 forOTP26–28. These support candidate future substrates, not tested installs/pins/matrix passes. In particular, review the current >=26→3.25 selection before adding29. No binary was installed by this documentation check.

## Workflow, models and ownership

Operator-selected Autonomous Mode uses two actual contributor contexts: CDC OpenAI GPT-6.1 Sol, High (planning, source-grounded review and independent reproduction); CC OpenAI GPT-6 Luna, High (bounded diagnosis/evidence assembly). These are explicit Operator choices relayed in the orchestration assignment; no fallback is authorized. Parent orchestration context retains integrated accountability and independently reviews arc composition. Model names alone do not establish independence.

Source audit checkout: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit`, branch `codex/project02-arc01-audit`, baseline `bcfd94a`. Planning: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/planning`, orphan branch `planning`. CDC owns planning except explicit sequential CC handoffs. CC owns generated outputs in the isolated source checkout while executing; no tracked source mutation. No overlapping installs/builds/scanners. Each slice uses reviewer-authored `cdc-verification.md` plus CDC `cdc-retrospective.md`; arc closure assembled by CDC remains subject to parent independent composition review and Operator gate.

## Evidence and validation contract

Audit date captured with `date +%Y.%m.%d`: `2026.10.31`. Per-slice durable artifacts override code-auditing's `workbench/` default under the repository/Operator standard. Keep distinct failed, retried, CC and CDC observations in existing run logs; no stale May build output counts as a current baseline. Exact commands, exit statuses, actual selected populations and limits are required. Source expected values come from documented contracts or independently derived calendar cases, not another call to the same implementation. Hang hypotheses use monitored workers with a timeout and kill/cleanup.

Actual preflight observed local rebar3 3.27.0 / OTP 28, outside configured OTP20–27. Revalidate in execution. CI runs compile/xref/Dialyzer on20–27 and tests/PropEr/coverage on21–27; R15B03 is intended historical floor, not demonstrated here. Rebar check/CI requires100% coverage; Make script requires95%; neither is lowered. Attempt applicable native gates in a fresh isolated checkout; preserve failures. If dependency loading is unavailable, disclose blocked gates and use fresh local erlc/erl public-API probes as bounded evidence, never a replacement claim of full CI success.

## Historical migration independent check

CDC independently compared all12 archived files against source bytes, SHA256 and source/archive nanosecond mtimes; all match. All30 historical acceptance rows across9 ledgers remain OPEN; exactly2 arcs/6 slices. All81 authored relative links resolve; all14 task/mainline patch-ID pairs match; planning has one parentless root. History remains historical and is not accepted retroactively. Migration baseline `213bfc7`; these checks were read-only and no historical file was changed.

## Known uncleared watch items

Arc02 owns public-type/tooling reconciliation, including `parse_exact/1` src129 returning`calendar:datetime()` in its spec while its documented/exercised result has fractional second6.5 for`20120203T040506.50`. Private`datetime()` src40 permitsfloat; calendar second() is integer0..59. This source-pinned mismatch was observed during Slice01 review and remains uncleared for Arc02 severity/fix-compatibility assessment; it is not silently accepted or repaired in Arc01.

Slice02 explicitly leaves the reduced-precision zoned-start contract question uncleared for Arc02 public-contract/test reconciliation: `parse_interval("2007-12-14T13:30+02:00/15:30:00")` raisesbadarg before inheritance, while fullseconds succeeds (source299–387). Current README/tests do not separately promise this form; observed rejection is not a confirmed defect or standards conclusion. Arc02 must disposition that evidence rather than silently clear it. Zero-duration bareP serialization, by contrast, was reviewed against historical omission/always-P design and is not counted as a defect.

Arc01 now has9 stable findings (3High/5Medium/1Low) at unchangedbcfd94a; both slices independently accepted, exact15-export/74-form helper map assembled. This is runtime-only delivery; independent parent composition and Operator decision remain open. The v1.2 CI exception superseded the original all-project repair prohibition only for later OTP28/29 compatibility, without extending current execution.

## Definition of done

See sibling `ledger.md`. Project completion requires all three arcs, complete mapped scope, independent evidence and explicit Operator acceptance. Arc01 findings are not a claim that the whole project or prior release is accepted.

## Version history

- v1.0 — 2026-10-31: CDC opened two-arc diagnosis roadmap after byte-preserved historical migration; only Arc01 execution authorized.

- v1.1 — 2026-10-31: Arc01/Slice01 review explicitly identified the known parse_exact return-spec obligation for already-roadmapped Arc02; no new arc or source scope added.

- v1.2 — 2026-10-31: direct Operator request added roadmap Arc03 for OTP28/29 CI and only necessary compatibility fixes, preserving existing support and the current Arc01 stop gate. Parent primary-document provenance retained above; no current configuration change.

- v1.3 — 2026-10-31: Arc01/Slice02 close surfaced the explicit reduced-precision-zone contract obligation, retained zero-duration intent disposition and runtime delivery status. No new execution or repair scope; parent/human gates stay open.
