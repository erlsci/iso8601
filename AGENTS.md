# AGENTS.md — iso8601

Guidance for coding agents working in this repository. `CLAUDE.md` is a symlink
here; edit this file rather than maintaining a second set of instructions.

## Library and public contract

iso8601 is a small Erlang library for ISO 8601 date formatting, parsing,
durations, and intervals. Production code lives in `src/iso8601.erl`; the OTP
application depends only on `kernel` and `stdlib`. Read `src/iso8601.app.src`
for the current version and `README.md` for usage.

Public functions are `format/1`, `parse/1`, `parse_exact/1`, `add_time/4`,
`subtract_time/4`, `add_days/2`, `add_months/2`, `add_years/2`,
`parse_duration/1`, `apply_duration/2`, `parse_interval/1`, `interval_bounds/1`,
`format_interval/1`, `parse_expanded/1`, and `format_expanded/1`.

Exported types are `timestamp/0`, `year/0`, `month/0`, `day/0`, `hour/0`,
`minute/0`, `second/0`, `duration/0`, `interval/0`, `expanded_year/0`, and
`expanded_datetime/0`. The calendar-style types are intentionally convenient
for callers; keep them exported. This is a stable public API: preserve return
shapes, accepted inputs, error behavior, and type compatibility unless a
breaking change is explicitly agreed and versioned under SemVer.

Expanded years and intervals are implemented. Consult their documented limits,
current source, and relevant issues before claiming an unsupported feature or
changing behavior. This library does not need a supervision tree.

## Compatibility and Erlang conventions

- Preserve broad OTP compatibility. CI configures OTP 20–29; OTP 20 runs compile,
  xref, and Dialyzer only. EUnit, PropEr, and coverage are configured on OTP 21–29.
  The historical library build target extends to R15B03; this is an intended
  compatibility floor, not proof of validation on that release. Inspect existing
  BIF and syntax use before introducing a newer requirement.
- Use EDoc; do not introduce OTP-27 `-doc`/`-moduledoc` attributes. Keep
  `undefined_or`, avoiding the `maybe` keyword collision.
- Keep helper records private. Do not introduce shared records, `_new` forks,
  or macros that wrap logic. Existing exceptions are audit inputs, not automatic
  permission for breaking cleanup.
- Use the installed `collaboration-framework` and `erlang-guidelines` skills.
  The Erlang skill's OTP-27 defaults yield to this repository's older floor.
  Removed `priv/ai` guidance is not an available dependency.
- Fix compiler warnings even though `erl_opts` does not enable
  `warnings_as_errors`. Prefer specific failure contracts over blanket catches.

## Planning and collaboration

- Plans and durable evidence live on the orphan `planning` branch in
  `.worktrees/planning/`, under `projectNN-<slug>/`. Use `project-plan.md`,
  `arc-plan.md`, `slice-plan.md`, and a sibling `ledger.md` at each scale.
  Durable slice outputs default to the owning slice's `artifacts/` directory.
  Read the planning worktree's instructions and the active plan before acting.
- Preserve historical records and their provenance. Old prompts and audit
  claims are evidence, not current assignments or independently accepted facts.
  `workbench/` is ignored operator scratch; do not use it as the sole home of
  durable project records or normalize archived originals.
- Use the peer frame: contribute judgment, flag uncertainty and disagreements,
  and distinguish proposed-done from independent verification and Operator
  acceptance. Never represent self-review as independent review.
- Inspect `git status` and `git worktree list` before mutations. Preserve
  unrelated changes. Autonomous implementation uses a dedicated non-default
  branch/worktree; `main` is the integration branch. Pause at agreed arc gates
  for Operator review and merge. Local commit authority does not authorize push,
  merge, release, or publication.

## Build and validation

Read `rebar.config`, `Makefile`, and `.github/workflows/ci.yml` for the actual
commands and gates. Useful commands from the source worktree:

- `rebar3 compile` — compile production code.
- `rebar3 xref` — configured undefined, unused, and deprecated-call checks.
- `rebar3 dialyzer` — includes unknown, unmatched-return, error-handling, and
  underspec warnings.
- `rebar3 eunit -c -v` — EUnit tests in `test/iso8601_tests.erl`.
- `rebar3 proper -c` — properties in `test/prop_iso8601.erl`.
- `rebar3 as test cover -v --min_coverage=100` — configured CI coverage gate.
- `rebar3 check` — alias for compile, xref, Dialyzer, EUnit, and coverage;
  the coverage alias runs PropEr and requires 100%.
- `make check` — a different gate: clean, compile, formatting check, xref,
  Dialyzer, lint, EUnit/Common Test/PropEr, and the executable-line coverage
  script. That script currently requires 95%. Report this discrepancy honestly;
  do not lower either gate to obtain a pass. `make clean` removes generated
  build, log, crash-dump, and documentation output; establish ownership first.

Dev tools run under `rebar3 as dev`: `fmt -w`, `fmt --check`, `lint`, `ex_doc`,
and `hex publish`. They are separate from the default build/test dependencies.
Release work requires explicit authorization: SemVer, Git history, published
release notes, and Hex publication; no hand-maintained `CHANGELOG`.
CI runs on `main`, `release/**`, `task/**`, `feature/**`, `epic/**`, and tags.
A `codex/**` branch needs a qualifying PR for that workflow to run; do not
claim a local check reproduces the entire OTP matrix.

Before submitting implementation, run applicable compile, xref, Dialyzer,
EUnit, PropEr, coverage, formatting, and lint gates; self-review against the
Erlang skill and check API/type compatibility. Record exact failures, unrun
checks, and environmental limits rather than implying a full pass.
Documentation-only edits need proportionate content, link, and diff checks.

## Commit message guidance

Commit only intended files using explicit paths and `git commit --only` when
other work may be present. Every assistant-authored commit must include both
trailers in one contiguous trailer block:

```text
Co-authored-by: Codex <noreply@openai.com>
Co-authored-by: Billo AI <ai-engineering@billo.systems>
```
