# CLAUDE.md — iso8601

Standing instructions for Claude (including Claude Code / "CC") working in this
repository. Auto-loaded each session. Keep it short; keep it followed.

## What iso8601 is

iso8601 is an ISO 8601 date formatting and parsing library for Erlang. It is a
small, stable, single-module library (`src/iso8601.erl`, app `iso8601`,
currently v1.3.4) — not an application with a supervision tree. It depends only
on `kernel` and `stdlib`.

History: originally `erlang_iso8601`, created by Sean Sawyer in 2011; handed to
the [erlsci](https://github.com/erlsci) org in 2016 and renamed to `iso8601`.
Maintained by Duncan McGreggor. Published on Hex; mirrored to GitHub and GitLab.

Public API (all in `iso8601`): `format/1`, `parse/1`, `parse_exact/1`,
`add_time/4`, `subtract_time/4`, `add_days/2`, `add_months/2`, `add_years/2`,
`parse_duration/1`, `apply_duration/2`. The module also exports the `calendar`-style
types (`year/0`, `month/0`, `day/0`, `hour/0`, `minute/0`, `second/0`,
`timestamp/0`) **on purpose, as a convenience for callers** — keep them exported;
do not hide them behind opaque accessors.

Known deficiencies (see open issues before "fixing"): no expanded year
representation; no interval support.

## House style — load this first

Before writing or reviewing Erlang, read **`priv/ai/erlang/SKILL.md`** and follow
its own loading instructions (it indexes `priv/ai/erlang/guides/`). This is the
authoritative Erlang skill and the operative code-quality reference. (It is a
symlink into the shared `ai-engineering` knowledge base, used across the erlsci
Erlang projects.)

Note that the official home for this SKILL and guides is here:

* <https://github.com/billosys/ai-engineering>

## Conventions

* This is a public, widely-depended-on library with a stable API. **Preserve
  backward compatibility** unless a breaking change is explicitly agreed and the
  version bumped accordingly (SemVer). Don't break callers casually.
* **Broad OTP support.** CI exercises OTP 20–27; the library is meant to build as
  far back as R15B03. Don't introduce features newer than the floor you're keeping
  (e.g. the OTP-27 `maybe` keyword clash is why the type is named `undefined_or`,
  not `maybe`). No `-doc`/EEP-48 attributes; use edoc.
* **No shared records across module boundaries.** (There's effectively one module
  here, but keep any helpers' records private.)
* **No `_new` forks; no macros for logic.** One way to do a thing.
* **Release discipline:** SemVer + git history + published release notes / Hex.
  No hand-maintained `CHANGELOG`.

## Tooling

Build and check with rebar3 (Makefile wraps the common ones):

* `rebar3 compile` (`make build`)
* `rebar3 xref` — `xref_checks` configured in `rebar.config`.
* `rebar3 dialyzer` — warnings: `unknown`, `unmatched_returns`, `error_handling`,
  `underspecs`.
* `rebar3 eunit -v` (`make check`) — tests live in `test/iso8601_tests.erl`.
* `rebar3 as test do proper -c, cover -v --min_coverage=0` — PropEr properties +
  coverage. Coverage currently has **no hard gate** (`--min_coverage=0`); aim to
  raise real coverage rather than gaming the number.
* `rebar3 check` — the full local gate: compile, xref, dialyzer, eunit, coverage.

Note `erl_opts` is empty — `warnings_as_errors` is **not** on. Still, treat
compiler warnings as defects to fix, not noise to tolerate.

## How we work (process rigour)

Two roles. **CC** implements and self-assesses. A separate reviewer context
independently verifies — re-running commands and reading diffs, not summaries.
The implementer does not mark its own work verified.

Write to the floor, not the ceiling: state what the work actually achieves, name
what is not done, and distinguish "verified by running X" from "I believe X".

## Subagent Delegation Policy

(full text: `priv/ai/SUBAGENT-DELEGATION-POLICY.md`)

* **Do not delegate thinking work to subagents** — code edits, design/architecture
  decisions, tradeoff reasoning, judging whether a finding is real, planning,
  evaluating correctness.
* **Subagents are for lookup only** — finding files/symbols, grepping, reading a
  file, fetching docs: retrieval that needs no judgment about the result.
* Serial on thinking (main context); parallel on lookup. Quality over wall-clock
  on the thinking path.

## Branches & CI

* Work on **`main`** (default branch). Long-lived feature work can use
  `feature/**`, `epic/**`, `release/**`, or `task/**` branches.
* CI (`.github/workflows/ci.yml`) fires on `main`, `release/**`, `task/**`,
  `feature/**`, `epic/**`, and tags. It is the independent reproducer for
  compile/xref/dialyzer/eunit/coverage across the OTP 20–27 matrix.
* Releases push to GitHub and GitLab and publish to Hex (`rebar3 as dev hex publish`,
  via `make publish`). Dev/doc/publish plugins (erlfmt, lint, ex_doc, hex, coveralls)
  live in the `dev` rebar3 profile, so invoke them as `rebar3 as dev <task>`.

## Collaboration posture

Peer frame: equal contributors, mutual intellectual humility, honest engagement
over agreeable hedging. Being corrected is a contribution, not a defeat. See
`priv/ai/AI-CONSTITUTION-SUPPLEMENT.md` and `priv/ai/AI-ENGINEERING-METHODOLOGY.md`.

## Before submitting

* [ ] `rebar3 compile` clean (fix all warnings, even though they aren't errors).
* [ ] `rebar3 xref` clean.
* [ ] `rebar3 eunit` green; PropEr properties pass.
* [ ] `rebar3 dialyzer` clean.
* [ ] Public API and exported types unchanged (or change is intentional + versioned).
* [ ] Self-reviewed against the Erlang skill (`priv/ai/erlang/SKILL.md`).
