# Slice01 CC run log

## Assignment and boundary

Executed the initial evidence-only CC assignment at planning commit `e2b11d8a7ace3d3902e275ccb3690608141034ec`, from `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/planning/project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/cc-prompt.md`. The embedded **Required-reading manifest** is the manifest; no separate manifest file is specified. Date API scope is `format/1`, `parse/1`, `parse_exact/1`, `parse_expanded/1`, `format_expanded/1` and the shared date/format helpers named by the prompt. Source diagnosis only: no source, test, or build-configuration changes authorized. CC owns artifacts, this run log, the closing report, and evidence/status fields in the Slice01 ledger. CDC owns independent reproduction and retrospective.

## Preflight snapshot

- `date +%Y.%m.%d`: `2026.10.31`.
- Source worktree: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit`; branch `codex/project02-arc01-audit`; `HEAD=bcfd94a08fa7a427eb1469679897e4d92d808edf`; `git status --short --branch` printed only `## codex/project02-arc01-audit` (clean).
- Planning worktree: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/planning`; branch `planning`; `HEAD=e2b11d8a7ace3d3902e275ccb3690608141034ec`; status printed only `## planning` (clean before this assigned work).
- Source tracked-file population: `git ls-files | wc -l` = 16. This was reconciled to the project map, with no unexplained population delta.
- Fresh build ownership: source `_build` absent (`test -e` false); no May-era beam/cache output was imported or treated as behavioral evidence.
- Runtime: `erl -version` reported emulator `16.1.1`; `rebar3 --version` reported `rebar 3.27.0 on Erlang/OTP 28 Erts 16.1.1`.

## Required full reads (document reads, not search queries)

All source files below were read in full at baseline `bcfd94a08fa7a427eb1469679897e4d92d808edf`; the line ranges below are the full visible ranges. Source lengths were confirmed with `wc -l`:

- `README.md` 1–239; `AGENTS.md` 1–110; `src/iso8601.erl` 1–744; `src/iso8601.app.src` 1–21.
- `test/iso8601_tests.erl` 1–1069 (contiguous recovered reads: 1–280, 281–560, 561–840, 841–1069); `test/prop_iso8601.erl` 1–111.
- `rebar.config` 1–89; `Makefile` 1–96; `.github/workflows/ci.yml` 1–92; `elvis.config` 1–23; `scripts/check_coverage.escript` 1–91.

Planning documents were read in full at planning `e2b11d8a7ace3d3902e275ccb3690608141034ec`:

- Planning `AGENTS.md` 1–16; Project02 `project-plan.md` 1–61 (required sections: Outcome and boundaries; Audit architecture and complete map; Roadmap; Workflow/models/ownership; Evidence/validation); Arc01 `arc-plan.md` 1–24; Slice01 `slice-plan.md` 1–27; this assignment `cc-prompt.md` 1–49; Slice01 `ledger.md` 1–12. Arc01 plan was read in full. Historical archives were not treated as instructions and the old report was not audited.

Framework/engineering-methods reads, full entrypoint and required sections:

- `collaboration-framework/SKILL.md` 1–574 (contiguous recovered reads: 1–200, 201–400, 401–574).
- `knowledge/engineering-methods/guides/01-engineering-methodology.md`: Notes for Codex, Roles/shared invariants, Two-Contributor Workflow, Autonomous Mode, and Codex Interpretation; read as complete bounded sections at visible lines 26–153, 326–430, 429–630.
- `knowledge/engineering-methods/guides/07-implementation-prompt-authoring.md`: Required reading and CC intake, lines 297–398.
- `knowledge/project-management/guides/README.md`: Notes for Codex lines 1–70; Expedited Mode 94–129; Autonomous Mode 130–176.

Erlang-guidelines entrypoint and all 17 direct guide chapters were read fully; chapter 11 was first, then 01–10 and 12–17. Whole-file line ranges confirmed with `wc -l`:

- `erlang-guidelines/SKILL.md` full entrypoint; `guides/11-anti-patterns.md` 1–571; `01-core-idioms.md` 1–489; `02-api-design.md` 1–355; `03-error-handling.md` 1–440; `04-data-and-types.md` 1–479; `05-functions-and-pattern-matching.md` 1–443; `06-processes-and-concurrency.md` 1–446; `07-otp-behaviours.md` 1–467; `08-supervision-and-applications.md` 1–452; `09-fault-tolerance.md` 1–439; `10-performance.md` 1–502; `12-project-structure.md` 1–354; `13-documentation.md` 1–355; `14-production-ops.md` 1–388; `15-testing.md` 1–355; `16-distribution.md` 1–368; `17-tooling.md` 1–317. One initial combined output truncated; every affected chapter was recovered in bounded contiguous full reads before audit work.

Code-auditing, evidence, and close guidance:

- `code-auditing/SKILL.md` 1–43; `guides/01-audit-scope-and-map.md` 1–165; `02-findings-and-severity.md` 1–124; `03-scale-aware-auditing.md` 1–101; all read in full.
- `work-verification/SKILL.md` 1–51; `guides/01-ledger-discipline.md` 1–115; `02-evidence-strength.md` 1–72; `03-row-closure.md` 1–81; all read in full.
- Project-management `guides/04-closing-slices.md` read in full, 1–122; required sections “The slice closing-report” and “The slice bubble-up check” were applied. CC reports attested/proposed-done only; CDC verification and retrospective remain reviewer-owned.

## Queries distinct from full reads

- `wc -l` established the actual complete-read ranges above; it was not used as a substitute for reading.
- A heading search confirmed the required named Project02 sections; a source/test `rg -n` query located public APIs, types, date helpers, README contract passages, and date-test witnesses. Numbered source inspection then displayed `src/iso8601.erl` 27–156, 236–505, and 544–555. The targeted query is navigation evidence only; full reads above establish context.
- No web/standard claim has yet been introduced. ISO/version/deprecation claims not settled by repository evidence will be checked against installed OTP or authoritative primary material before being treated as findings.

## Source-cited contract readback (before evidence generation)

`src/iso8601.erl` exports the ordinary calendar-style types plus `expanded_year()` and `expanded_datetime()` (lines 27–54); ordinary `year()` is non-negative and `month()/day()/hour()/minute()/second()` are broad calendar component ranges. `format/1` accepts a datetime or timestamp and declares `binary()` output (95–108); float seconds are rendered with fixed six-decimal formatting (101–104). `format_expanded/1` uses signed/padded expanded-year rendering and also returns `binary()` (110–119, 549–555). The three parse entrypoints declare `iodata()` and calendar or expanded datetime results (121–147); binary inputs are converted to lists, ordinary parse discards the fractional seconds component via `trunc/1`, whereas `parse_exact/1` and `parse_expanded/1` preserve the separate seconds fraction (123–147). The parser collects explicit signed expanded years only for `parse_expanded/1` and rejects `-` under positive-only entrypoints (236–253). Its dispatch supports ordinal, week, month/day and basic forms (255–353); ordinal conversion checks the day count against Gregorian leap-year month lengths (398–427). `datetime/1` applies fractional hour/minute adjustments and then UTC offset through `apply_offset/4` (429–441); the latter rounds offsets to whole seconds, converts positive Gregorian years through calendar BIFs, preserves negative-year datetimes only for zero rounded offset, and otherwise raises `badarg` (491–505). ISO week conversion derives week 1 from January 1's weekday (471–489). README confirms microsecond fractional seconds via `parse_exact/1` and precision caveats (lines 51–53), binary/list parse examples (73–108), expanded-year behavior and negative-year UTC/week-offset limitations (166–192). Existing tests witness leap/ordinal acceptance/rejection (196–246), float formatting and binary inputs (306–323), negative and expanded-year behavior (927–1067). These witnesses do not establish every calendar-validity or formatting boundary; public probes below compare behavior against this documented return/error contract and independent date arithmetic.

## Next action

Proceed to focused public-API hypotheses and controlled probes, then bounded validation attempts. Do not change source bytes; retain every attempt, including blocked gates, in the artifacts and close packet.

## Ledger terminology clarification

CDC confirmed that S01-02's “negative findings” is the code-auditing guide's defined term for checks that came back clean; its verification phrase calls for at least five concrete clean checks, not five defects. The ledger note “Zero findings is allowed if supported” and the prompt's “at least five concrete clean checks” / “No finding quota” agree. No ledger amendment or scope change is needed. This report has four supported findings and six scoped clean checks.

## Public probe record

Runner: `artifacts/date-probes.escript`, loaded only `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit/_build/default/lib/iso8601/ebin/iso8601.beam` freshly produced at this pass. Command: `artifacts/date-probes.escript > artifacts/date-probes.out`; exit 0. SHA-256: runner `e3ee17e072e223fecdf4fd8fda67d40081907395fe38fbe652c86477055a105c`; transcript `aada941cc7dc953330b6ae5b2fae551c60cd7bc972d760190f0c8fe3ac81a76a`.

Key comparisons and independent controls (the full term transcript is in `date-probes.out`):

- **Week boundaries:** valid `2009-W53-7` returned 2010-01-03; valid `2015-W01-1` returned 2014-12-29. Invalid `2014-W00-1` returned 2013-12-23; invalid `2014-W53-1` returned 2014-12-29; invalid `2014-W01-8` returned 2014-01-06. `calendar:iso_week_number({2014,12,29})` returned `{2015,1}` on OTP 28, independently confirming the alias. Erlang/OTP 18 calendar docs define `daynum() = 1..7`, `weeknum() = 1..53`, and state that its week-number calculation conforms to ISO 8601; the module exports `iso_week_number/1` since OTP R14B02 ([OTP 18 calendar manual](https://www.erlang.org/docs/18/man/calendar.html), lines 1404–1439, 1507–1513). ISO's current published overview identifies Gregorian dates and 24-hour times as its scope ([ISO 8601-1:2019 overview](https://committee.iso.org/standard/70907.html), reviewed/confirmed 2024, lines 9–24, 45–49). The exact year-specific boundary was checked by local OTP's independent `calendar:iso_week_number/1`, not inferred from parser helper agreement.
- **Expanded-date validation:** valid `-0001-01-01`, `-0004-02-29`, and year-zero leap date `-0000-02-29` returned. Invalid `-0001-02-29`, `-0001-02-30`, `-0001-13-01`, `-0001-01-00`, and `-0000-02-30` all returned tuples. Positive control `parse("2017-02-30")` raised `error:if_clause` (the existing test at `test/iso8601_tests.erl:1043–1047` expects an error for an out-of-range positive calendar date). The source branch at `src/iso8601.erl:497–505` bypasses Gregorian calendar conversion for every year below 1 when rounded offset is zero; `make_date/2` constructs a month date without checking its day at `468–475`. OTP 18 docs state all calendar dates conform to Gregorian rules and date tuples must be valid (manual lines 1404–1418), with Gregorian leap rule at 1643–1651. This validates expectation without relying on old audit claims. Parent's archived-history note confirms nonzero offsets for years <=0 intentionally raise `badarg`; that behavior is not reported as a finding.
- **Input shape:** flat string and binary controls returned expected values. Three valid nested-iodata inputs, one each to `parse/1`, `parse_exact/1`, and `parse_expanded/1`, raised `error:badarg`. The type specs say `iodata()` at `src/iso8601.erl:121–147`, while implementation converts only binary inputs and passes other values directly into flat-list parser clauses (`123–147`, parser clauses `236–388`). README/EDoc examples call inputs string/binary (README 73–108; source EDoc 122–142), so the finding is specifically an exported-type/documentation mismatch; the three functions' declared types are broader than their prose examples.
- **Float rollover:** `format({{2012,12,30},{23,59,59.9999994}})` returned `...59.999999Z`; changing only the input to `59.9999999` returned `...60.000000Z`. Parsing the latter returned `{{2012,12,31},{0,0,0}}`. The expanded formatter produced `-0001-12-30T23:59:60.000000Z` on the analogous input. NIST's official table lists the only 2012 UTC leap-second insertion at 2012-06-30 (no 2012-12-30 insertion; all listed insertion dates are month-end) ([NIST leap seconds](https://www.nist.gov/pml/time-and-frequency-division/time-realization/leap-seconds), lines 152–173); IERS describes insertions at the end of June or December ([IERS Bulletin C metadata](https://datacenter.iers.org/productMetadata.php?id=16)). The test date therefore cannot explain `60` as a real leap second.
- Prompt baseline positive controls all matched: ordinary parse, exact fractional parse, positive expanded year through ordinary parse, negative expanded parse, rejection of negative ordinary parse, ordinal leap day, week-1 Monday, and epoch timestamp formatting.

## Concrete clean checks (scope-limited, not whole-project claims)

1. All five assigned public API entrypoints were inspected end-to-end, specs were present, and one or more real public calls exercised each.
2. Ordinal parsing has explicit `DDD > 0`, leap-year day table, and exhausted-year rejection at `src/iso8601.erl:398–427`; tests cover invalid day zero, leap-year day 366 acceptance, and non-leap day 366 rejection (`test/iso8601_tests.erl:196–247`). Public ordinal leap-day probe returned Feb 29.
3. Ordinary negative-year rejection is an explicit `badarg` branch (`src/iso8601.erl:236–245`) and matched test/README examples; expanded negative UTC zero-offset parsing is explicitly documented and returned a negative year. The intentional nonzero-offset restriction is not mislabeled as an unintended failure.
4. The scoped production module scan found no `list_to_atom`, `binary_to_term`, `term_to_binary`, `catch`, `try`, `io:format`, or `io:put_chars`. Thus no dynamic atom conversion, external-term decoding, blanket swallowed exceptions, or debug-output path was found in this module.
5. `++` matches are limited to interval-string assembly and duration-format assembly at source lines 668–677 and 731–742, outside the five date parser/formatter paths. No accumulating-append anti-pattern was found in assigned date paths.
6. The module has no process/OTP behavior/service/deployment/distribution code; it is a synchronous library depending only on `kernel` and `stdlib` (`src/iso8601.app.src`, full read). Process/supervision/distribution categories are not applicable here; no supervisor was recommended.

## Native validation attempts

Each Rebar command was run once with a Python `subprocess.run(..., timeout=45)` wrapper because this shell does not provide the `timeout` utility. Exact tool command, exit, and salient log output:

| Tool command | Exit | Observed output |
|---|---:|---|
| `rebar3 compile` | 0 | First wrapper attempt `timeout 45s rebar3 compile` exited 127 before invoking Rebar (`timeout: command not found`). Bounded Python-wrapped command then reported Fetching `rebar3_proper v0.12.1`; network download error and cached Hex archive fallback; compiled plugin and `iso8601` successfully. No prior source-worktree `_build` existed. |
| `rebar3 xref` | 0 | Verified dependencies, compiled `iso8601`, ran cross-reference analysis; no findings printed. |
| `rebar3 dialyzer` | 0 | Updated/read OTP28 PLT (222 files), analyzed one project module; no warnings printed. |
| `rebar3 eunit -c -v` | 0 | Fetched `proper v1.5.0`; network download error and cached Hex archive fallback; EUnit output: all 210 tests passed. |
| `rebar3 proper -c` | 0 | Four properties each passed 100 tests; `4/4 properties passed`. |
| `rebar3 as test cover -v --min_coverage=100` | 0 | `iso8601` and total 100%; coverdata included both EUnit and PropEr runs. |
| `rebar3 as dev fmt --check` | 1 | Plugin updates failed for erlfmt, rebar3_lint, rebar3_ex_doc and coveralls; command ended `Command fmt not found`. No source formatting conclusion. |
| `rebar3 as dev lint` | 1 | Same plugin update failures; command ended `Command lint not found`. No source lint conclusion. |

The dev plugin calls were not repeated because their fetch/update prerequisite failed unchanged. `rebar3 check` and `make check` umbrella aliases were not separately invoked after each pertinent rebar gate was run; they are not claimed as passed. `Makefile`'s distinct coverage script threshold is 95%, while CI/rebar coverage gate is 100%; both remain unchanged. OTP matrix other than installed OTP 28 was not exercised. The initial native `compile` wrapper failure is a shell utility availability limit, followed immediately by one successful bounded native compile, not a failed compile gate.

### Preserved raw gate output (failure and population receipts)

The first attempted bounded compile wrapper was `timeout 45s rebar3 compile`; it did not invoke Rebar. Exact shell result:

```text
/opt/homebrew/bin/bash: line 1: timeout: command not found
exit=127
```

The actual bounded command was then run once as `python3 -c 'import subprocess,sys; p=subprocess.run(["rebar3","compile"],capture_output=True,text=True,timeout=45); print(p.stdout,end=""); print(p.stderr,end="",file=sys.stderr); sys.exit(p.returncode)'`; exact Rebar stdout:

```text
===> Fetching rebar3_proper v0.12.1
===> Download error, using cached file at /Users/oubiwann/.cache/rebar3/hex/hexpm/packages/rebar3_proper-0.12.1.tar
===> Analyzing applications...
===> Compiling rebar3_proper
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling iso8601
exit=0
```

`rebar3 xref` stdout was `===> Verifying dependencies...`, `===> Analyzing applications...`, `===> Compiling iso8601`, `===> Running cross reference analysis...`; exit 0.

`rebar3 dialyzer` stdout was:

```text
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling iso8601
===> Dialyzer starting, this may take a while...
===> Add debug_info to compiler options (erl_opts) if Dialyzer fails to load Core Erlang.
===> Updating plt...
===> Resolving project files...
===> Updating base plt...
===> Resolving base files...
===> Checking 222 files in /Users/oubiwann/.cache/rebar3/rebar3_28.1.1_plt...
===> Copying /Users/oubiwann/.cache/rebar3/rebar3_28.1.1_plt to /Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit/_build/default/rebar3_28.1.1_plt...
===> Checking 222 files in _build/default/rebar3_28.1.1_plt...
===> Doing success typing analysis...
===> Resolving project warning files...
===> Analyzing 1 files with _build/default/rebar3_28.1.1_plt...
exit=0
```

`rebar3 eunit -c -v` exact dependency prelude and terminal totals:

```text
===> Verifying dependencies...
===> Fetching proper v1.5.0
===> Download error, using cached file at /Users/oubiwann/.cache/rebar3/hex/hexpm/packages/proper-1.5.0.tar
===> Analyzing applications...
===> Compiling proper
===> Analyzing applications...
===> Compiling iso8601
===> Performing EUnit tests...
...
[done in 0.637 s]
[done in 0.637 s]
[done in 0.637 s]
[done in 0.637 s]
module 'prop_iso8601'
=======================================================
  All 210 tests passed.
exit=0
```

`rebar3 proper -c` exact totals:

```text
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling iso8601
===> Testing prop_iso8601:prop_parse_format_roundtrip()
....................................................................................................
OK: Passed 100 test(s).
===> Testing prop_iso8601:prop_interval_start_end_roundtrip()
....................................................................................................
OK: Passed 100 test(s).
===> Testing prop_iso8601:prop_start_duration_bounds()
....................................................................................................
OK: Passed 100 test(s).
===> Testing prop_iso8601:prop_expanded_roundtrip()
....................................................................................................
OK: Passed 100 test(s).
===>
4/4 properties passed
exit=0
```

`rebar3 as test cover -v --min_coverage=100` exact coverage summary:

```text
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling iso8601
===> Performing cover analysis...
| module  | coverage |
| iso8601 | 100%     |
| total   | 100%     |
coverdata: /Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit/_build/test/cover/eunit.coverdata
coverdata: /Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit/_build/test/cover/proper.coverdata
summary: /Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit/_build/test/cover/index.html
exit=0
```

`rebar3 as dev fmt --check` stdout (exit 1), retaining fetch fallback diagnostic lines that appeared before the terminal command error:

```text
===> Failed to update package erlfmt from repo hexpm
===> Errors loading plugin erlfmt. Run rebar3 with DEBUG=1 set to see errors.
===> Failed to update package rebar3_lint from repo hexpm
===> Errors loading plugin rebar3_lint. Run rebar3 with DEBUG=1 set to see errors.
===> Failed to update package rebar3_ex_doc from repo hexpm
===> Errors loading plugin rebar3_ex_doc. Run rebar3 with DEBUG=1 set to see errors.
===> Fetching rebar3_hex v7.3.0
===> Download error, using cached file at /Users/oubiwann/.cache/rebar3/hex/hexpm/packages/rebar3_hex-7.3.0.tar
===> Fetching hex_core v0.19.0
===> Download error, using cached file at /Users/oubiwann/.cache/rebar3/hex/hexpm/packages/hex_core-0.19.0.tar
===> Fetching verl v1.1.1
===> Download error, using cached file at /Users/oubiwann/.cache/rebar3/hex/hexpm/packages/verl-1.1.1.tar
===> Analyzing applications...
===> Compiling verl
===> Compiling hex_core
===> Compiling rebar3_hex
===> Failed to update package coveralls from repo hexpm
===> Errors loading plugin coveralls. Run rebar3 with DEBUG=1 set to see errors.
===> Command fmt not found
exit=1
```

`rebar3 as dev lint` stdout (exit 1):

```text
===> Failed to update package erlfmt from repo hexpm
===> Errors loading plugin erlfmt. Run rebar3 with DEBUG=1 set to see errors.
===> Failed to update package rebar3_lint from repo hexpm
===> Errors loading plugin rebar3_lint. Run rebar3 with DEBUG=1 set to see errors.
===> Failed to update package rebar3_ex_doc from repo hexpm
===> Errors loading plugin rebar3_ex_doc. Run rebar3 with DEBUG=1 set to see errors.
===> Failed to update package coveralls from repo hexpm
===> Errors loading plugin coveralls. Run rebar3 with DEBUG=1 set to see errors.
===> Command lint not found
exit=1
```

Additional authoritative input-type check: OTP 18 `erlang` documentation defines `iodata()` as `iolist() | binary()` and demonstrates nested lists containing binaries and nested lists in `iolist_to_binary/1` ([OTP 18 erlang manual](https://www.erlang.org/docs/18/man/erlang.html)); OTP 24 states this definition directly ([OTP 24 erlang manual](https://www.erlang.org/docs/24/man/erlang.html)). The probe input is therefore valid declared iodata, not merely an informal nested string.

## Final pre-commit boundary check

Final source command sequence: `git diff --exit-code bcfd94a --` (exit 0, no output); `git status --short --branch` printed only `## codex/project02-arc01-audit`; `shasum -a 256 src/iso8601.erl` and `git show bcfd94a:src/iso8601.erl | shasum -a 256` both yielded `b3dd4c61be8b47ec0cd91439f1d8deeee1ef5e5e38d075be3358a893f9ad3350`. `_build` is ignored generated output from this fresh pass.

The actual 16 tracked source paths from `git ls-files`, reconciled against the Project02 map, are: `.github/workflows/ci.yml`, `.gitignore`, `AGENTS.md`, `CLAUDE.md` (symlink), `LICENSE`, `Makefile`, `README.md`, `elvis.config`, `priv/images/logo-large.png`, `priv/images/logo.png`, `rebar.config`, `scripts/check_coverage.escript`, `src/iso8601.app.src`, `src/iso8601.erl`, `test/iso8601_tests.erl`, `test/prop_iso8601.erl`.

The worktree inventory showed only the root `main`, planning worktree, and isolated audit source worktree; this handoff touched only the latter two. `git diff --check` passed in both source and planning worktrees. Before the close commit, planning status is limited to exactly this Slice01 `ledger.md`, `closing-report.md`, and the four listed artifacts; no other planning files are staged or changed. Source architecture-document discovery query `rg --files -g '*.md' -g '*.txt' -g '*.adoc'` returned only `AGENTS.md` and `README.md` in the isolated checkout; no separate architecture design document was found.
