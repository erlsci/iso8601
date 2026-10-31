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

## CDC independent review — initial CC packet3dc34cf, 2026-10-31

Source remains bcfd94a; commands below independently executed by CDC after CC ownership return. Existing unchanged native dependencies/OTP28 PLT were retained; probes and tests rerun. This is a current reproduction, not a fresh dependency resolution/OTP matrix.

### rebar3 compile

Exit: 0; elapsed 0.55s

```text
[0;32m===> Verifying dependencies...
[0m[0;32m===> Analyzing applications...
[0m[0;32m===> Compiling iso8601
[0m```

### rebar3 xref

Exit: 0; elapsed 1.79s

```text
[0;32m===> Verifying dependencies...
[0m[0;32m===> Analyzing applications...
[0m[0;32m===> Compiling iso8601
[0m[0;32m===> Running cross reference analysis...
[0m```

### rebar3 dialyzer

Exit: 0; elapsed 2.06s

```text
[0;32m===> Verifying dependencies...
[0m[0;32m===> Analyzing applications...
[0m[0;32m===> Compiling iso8601
[0m[0;32m===> Dialyzer starting, this may take a while...
[0m[0;35m===> Add debug_info to compiler options (erl_opts) if Dialyzer fails to load Core Erlang.
[0m[0;32m===> Updating plt...
[0m[0;32m===> Resolving project files...
[0m[0;32m===> Checking 222 files in _build/default/rebar3_28.1.1_plt...
[0m[0;32m===> Doing success typing analysis...
[0m[0;32m===> Resolving project warning files...
[0m[0;32m===> Analyzing 1 files with _build/default/rebar3_28.1.1_plt...
[0m```

### rebar3 eunit -c -v

Exit: 0; elapsed 1.32s

```text
[0;32m===> Verifying dependencies...
[0m[0;32m===> Analyzing applications...
[0m[0;32m===> Compiling iso8601
[0m[0;32m===> Performing EUnit tests...
[0m======================== EUnit ========================
file "iso8601.app"
  application 'iso8601'
    module 'iso8601'
      module 'iso8601_tests'
        iso8601_tests:12: -parse_fail_test_/0-fun-1- (fails to parse YYYYMM)...ok
        iso8601_tests:16: -parse_year_test_/0-fun-1- (parses YYYY)...[0.001 s] ok
        iso8601_tests:20: -parse_month_test_/0-fun-1- (parses YYYY-MM)...ok
        iso8601_tests:25: -parse_month_day_test_/0-fun-3- (parses YYYYMMDD)...ok
        iso8601_tests:26: -parse_month_day_test_/0-fun-1- (parses YYYY-MM-DD)...ok
        iso8601_tests:32: -parse_week_test_/0-fun-7- (parses 2009W01 as 2008-12-29)...ok
        iso8601_tests:33: -parse_week_test_/0-fun-5- (parses 2009-W01 as 2008-12-29)...ok
        iso8601_tests:34: -parse_week_test_/0-fun-3- (parses 2009W53 as 2010-01-03)...ok
        iso8601_tests:35: -parse_week_test_/0-fun-1- (parses 2009-W53 as 2010-01-03)...ok
        iso8601_tests:41: -parse_week_day_test_/0-fun-7- (parses 2009W011 as 2008-12-29)...ok
        iso8601_tests:42: -parse_week_day_test_/0-fun-5- (parses 2009-W01-1 as 2008-12-29)...ok
        iso8601_tests:43: -parse_week_day_test_/0-fun-3- (parses 2009W537 as 2010-01-03)...ok
        iso8601_tests:44: -parse_week_day_test_/0-fun-1- (parses 2009-W53-7 as 2010-01-03)...ok
        iso8601_tests:50: -parse_hour_test_/0-fun-3- (parses YYYYMMDDTHH)...ok
        iso8601_tests:51: -parse_hour_test_/0-fun-1- (parses YYYY-MM-DDTHH)...ok
        iso8601_tests:57: -parse_fractional_hour_test_/0-fun-7- (parses YYYYMMDDTHH.hh)...ok
        iso8601_tests:59: -parse_fractional_hour_test_/0-fun-5- (parses YYYY-MM-DDTHH.hh)...ok
        iso8601_tests:60: -parse_fractional_hour_test_/0-fun-3- (parses YYYYMMDDTHH,hh)...ok
        iso8601_tests:62: -parse_fractional_hour_test_/0-fun-1- (parses YYYY-MM-DDTHH,hh)...ok
        iso8601_tests:68: -parse_minute_test_/0-fun-3- (parses YYYYMMDDTHHMM)...ok
        iso8601_tests:69: -parse_minute_test_/0-fun-1- (parses YYYY-MM-DDTHH:MM)...ok
        iso8601_tests:76: -parse_fractional_minute_test_/0-fun-7- (parses YYYYMMDDTHHMM.mm)...ok
        iso8601_tests:78: -parse_fractional_minute_test_/0-fun-5- (parses YYYY-MM-DDTHHMM.mm)...ok
        iso8601_tests:80: -parse_fractional_minute_test_/0-fun-3- (parses YYYYMMDDTHHMM,mm)...ok
        iso8601_tests:82: -parse_fractional_minute_test_/0-fun-1- (parses YYYY-MM-DDTHHMM,mm)...ok
        iso8601_tests:88: -parse_second_test_/0-fun-3- (parses YYYYMMDDTHHMMSS)...ok
        iso8601_tests:90: -parse_second_test_/0-fun-1- (parses YYYY-MM-DDTHH:MM:SS)...ok
        iso8601_tests:97: -parse_fractional_second_test_/0-fun-7- (parses YYYYMMDDTHHMMSS.ss)...ok
        iso8601_tests:99: -parse_fractional_second_test_/0-fun-5- (parses YYYY-MM-DDTHHMMSS.ss)...ok
        iso8601_tests:101: -parse_fractional_second_test_/0-fun-3- (parses YYYYMMDDTHHMMSS,ss)...ok
        iso8601_tests:103: -parse_fractional_second_test_/0-fun-1- (parses YYYY-MM-DDTHHMMSS,ss)...ok
        iso8601_tests:110: -parse_exact_fractional_second_test_/0-fun-7- (parses YYYYMMDDTHHMMSS.ss)...ok
        iso8601_tests:112: -parse_exact_fractional_second_test_/0-fun-5- (parses YYYY-MM-DDTHHMMSS.ss)...ok
        iso8601_tests:114: -parse_exact_fractional_second_test_/0-fun-3- (parses YYYYMMDDTHHMMSS,ss)...ok
        iso8601_tests:116: -parse_exact_fractional_second_test_/0-fun-1- (parses YYYY-MM-DDTHHMMSS,ss)...ok
        iso8601_tests:122: -parse_fractional_fail_test_/0-fun-1- (fails to parses multiple decimals)...ok
        iso8601_tests:128: -parse_offset_test_/0-fun-5- (parses YYYYMMDDTHHMMSS.ssZ)...ok
        iso8601_tests:130: -parse_offset_test_/0-fun-3- (parses YYYYMMDDTHHMMSS.ss+0400)...ok
        iso8601_tests:132: -parse_offset_test_/0-fun-1- (parses YYYYMMDDTHHMMSS.ss+0400)...ok
        iso8601_tests:139: -parse_duration_test_/0-fun-7- (parses with pos sign)...ok
        iso8601_tests:152: -parse_duration_test_/0-fun-5- (parses without sign)...ok
        iso8601_tests:165: -parse_duration_test_/0-fun-3- (parses only years)...ok
        iso8601_tests:178: -parse_duration_test_/0-fun-1- (parses only minutes)...ok
        iso8601_tests:194: -parse_duration_fail_test_/0-fun-1- (fails to parses misspelled string)...ok
        iso8601_tests:200: -parse_ordinal_test_/0-fun-23- (parses YYYY-DDDTHHMMSS)...ok
        iso8601_tests:201: -parse_ordinal_test_/0-fun-21- (parses YYYY-DDD)...ok
        iso8601_tests:203: -parse_ordinal_test_/0-fun-19- (parses YYYY_DDDTHHMMSS, Feb 29 in a leap year)...ok
        iso8601_tests:205: -parse_ordinal_test_/0-fun-17- (parses YYYY_DDDTHHMMSS, after Feb 29 in a leap year)...ok
        iso8601_tests:207: -parse_ordinal_test_/0-fun-15- (parses YYYY_DDDTHHMMSS, Feb 29 in a leap century)...ok
        iso8601_tests:209: -parse_ordinal_test_/0-fun-13- (parses YYYY_DDDTHHMMSS, March 1 in a non-leap year)...ok
        iso8601_tests:211: -parse_ordinal_test_/0-fun-11- (parses YYYY_DDDTHHMMSS, after Feb 29 in a non-leap year)...ok
        iso8601_tests:213: -parse_ordinal_test_/0-fun-9- (parses YYYY_DDDTHHMMSS, March 1 in a non-leap century)...ok
        iso8601_tests:214: -parse_ordinal_test_/0-fun-7- (fails to parse ordinal date with 0 days)...ok
        iso8601_tests:216: -parse_ordinal_test_/0-fun-5- (fails to parse ordinal date with too many days in a leap year)...ok
        iso8601_tests:218: -parse_ordinal_test_/0-fun-3- (parses ordinal date with 366 days in a leap year)...ok
        iso8601_tests:220: -parse_ordinal_test_/0-fun-1- (fails to parse ordinal date with too many days in a non-leap year)...ok
        iso8601_tests:227: -parse_ordinal_exact_test_/0-fun-23- (parses YYYY-DDDTHHMMSS)...ok
        iso8601_tests:228: -parse_ordinal_exact_test_/0-fun-21- (parses YYYY-DDD)...ok
        iso8601_tests:230: -parse_ordinal_exact_test_/0-fun-19- (parses YYYY_DDDTHHMMSS, Feb 29 in a leap year)...ok
        iso8601_tests:232: -parse_ordinal_exact_test_/0-fun-17- (parses YYYY_DDDTHHMMSS, after Feb 29 in a leap year)...ok
        iso8601_tests:234: -parse_ordinal_exact_test_/0-fun-15- (parses YYYY_DDDTHHMMSS, Feb 29 in a leap century)...ok
        iso8601_tests:236: -parse_ordinal_exact_test_/0-fun-13- (parses YYYY_DDDTHHMMSS, March 1 in a non-leap year)...ok
        iso8601_tests:238: -parse_ordinal_exact_test_/0-fun-11- (parses YYYY_DDDTHHMMSS, after Feb 29 in a non-leap year)...ok
        iso8601_tests:240: -parse_ordinal_exact_test_/0-fun-9- (parses YYYY_DDDTHHMMSS, March 1 in a non-leap century)...ok
        iso8601_tests:241: -parse_ordinal_exact_test_/0-fun-7- (fails to parse ordinal date with 0 days)...ok
        iso8601_tests:243: -parse_ordinal_exact_test_/0-fun-5- (fails to parse ordinal date with too many days in a leap year)...ok
        iso8601_tests:245: -parse_ordinal_exact_test_/0-fun-3- (parses ordinal date with 366 days in a leap year)...ok
        iso8601_tests:247: -parse_ordinal_exact_test_/0-fun-1- (fails to parse ordinal date with too many days in a non-leap year)...ok
        iso8601_tests:254: -add_time_test_/0-fun-7- (add one second)...ok
        iso8601_tests:256: -add_time_test_/0-fun-5- (add one minute)...ok
        iso8601_tests:258: -add_time_test_/0-fun-3- (add one hour)...ok
        iso8601_tests:260: -add_time_test_/0-fun-1- (roll over to next day)...ok
        iso8601_tests:269: -subtract_time_test_/0-fun-7- (add one second)...ok
        iso8601_tests:271: -subtract_time_test_/0-fun-5- (add one minute)...ok
        iso8601_tests:273: -subtract_time_test_/0-fun-3- (add one hour)...ok
        iso8601_tests:275: -subtract_time_test_/0-fun-1- (roll back to previous day)...ok
        iso8601_tests:284: -add_months_test_/0-fun-11- (add one month in the middle of the year)...ok
        iso8601_tests:286: -add_months_test_/0-fun-9- (add one month at the beginning of the year)...ok
        iso8601_tests:288: -add_months_test_/0-fun-7- (add one month almost in the end of the year)...ok
        iso8601_tests:290: -add_months_test_/0-fun-5- (add one month in the end of the year)...ok
        iso8601_tests:292: -add_months_test_/0-fun-3- (add eight month in the middle of the year)...ok
        iso8601_tests:294: -add_months_test_/0-fun-1- (add twelve month in the middle of the year)...ok
        iso8601_tests:305: -format_test_/0-fun-5- (formats datetime with integer seconds)...ok
        iso8601_tests:307: -format_test_/0-fun-3- (formats datetime with float seconds)...ok
        iso8601_tests:308: -format_test_/0-fun-1- (formats timestamp tuple)...ok
        iso8601_tests:318: -parse_binary_test_/0-fun-5- (parse/1 accepts binary input)...ok
        iso8601_tests:320: -parse_binary_test_/0-fun-3- (parse_exact/1 accepts binary input)...ok
        iso8601_tests:324: -parse_binary_test_/0-fun-1- (parse_duration/1 accepts binary input)...ok
        iso8601_tests:335: -timestamp_input_test_/0-fun-7- (add_time/4 accepts timestamp tuple)...ok
        iso8601_tests:337: -timestamp_input_test_/0-fun-5- (add_days/2 accepts timestamp tuple)...ok
        iso8601_tests:339: -timestamp_input_test_/0-fun-3- (add_months/2 accepts timestamp tuple)...ok
        iso8601_tests:341: -timestamp_input_test_/0-fun-1- (add_years/2 accepts timestamp tuple)...ok
        iso8601_tests:352: -add_days_test_/0-fun-3- (add one day)...ok
        iso8601_tests:354: -add_days_test_/0-fun-1- (add days across month boundary)...ok
        iso8601_tests:361: -add_years_test_/0-fun-3- (add one year)...ok
        iso8601_tests:363: -add_years_test_/0-fun-1- (add ten years)...ok
        iso8601_tests:374: -add_months_edge_test_/0-fun-3- (add zero months)...ok
        iso8601_tests:376: -add_months_edge_test_/0-fun-1- (add month rolls back invalid day (Jan 31 + 1 month))...ok
        iso8601_tests:387: -apply_duration_test_/0-fun-3- (apply duration with all components)...ok
        iso8601_tests:391: -apply_duration_test_/0-fun-1- (apply simple year duration)...ok
        iso8601_tests:401: -parse_week_dow_test_/0-fun-11- (W01-1 year starting Monday (2018))...ok
        iso8601_tests:403: -parse_week_dow_test_/0-fun-9- (W01-1 year starting Tuesday (2019))...ok
        iso8601_tests:405: -parse_week_dow_test_/0-fun-7- (W01-1 year starting Wednesday (2014))...ok
        iso8601_tests:406: -parse_week_dow_test_/0-fun-5- (W01-1 year starting Friday (2010))...ok
        iso8601_tests:408: -parse_week_dow_test_/0-fun-3- (W01-1 year starting Saturday (2011))...ok
        iso8601_tests:409: -parse_week_dow_test_/0-fun-1- (W01-1 year starting Sunday (2012))...ok
        iso8601_tests:420: -parse_offset_format_test_/0-fun-3- (offset hour only (+04))...ok
        iso8601_tests:422: -parse_offset_format_test_/0-fun-1- (offset with colon (+04:30))...ok
        iso8601_tests:433: -parse_colon_fractional_minute_test_/0-fun-3- (parses HH:MM.mm (dot))...ok
        iso8601_tests:435: -parse_colon_fractional_minute_test_/0-fun-1- (parses HH:MM,mm (comma))...ok
        iso8601_tests:442: -parse_colon_fractional_second_test_/0-fun-3- (parses HH:MM:SS.ss (dot))...ok
        iso8601_tests:444: -parse_colon_fractional_second_test_/0-fun-1- (parses HH:MM:SS,ss (comma))...ok
        iso8601_tests:454: -parse_error_clauses_test_/0-fun-21- (year too short)...ok
        iso8601_tests:455: -parse_error_clauses_test_/0-fun-19- (month_or_week trailing dash)...ok
        iso8601_tests:456: -parse_error_clauses_test_/0-fun-17- (week_day bad char after week)...ok
        iso8601_tests:457: -parse_error_clauses_test_/0-fun-15- (month_day short day)...ok
        iso8601_tests:458: -parse_error_clauses_test_/0-fun-13- (month_day_no_hyphen single char day)...ok
        iso8601_tests:459: -parse_error_clauses_test_/0-fun-11- (hour incomplete after T)...ok
        iso8601_tests:460: -parse_error_clauses_test_/0-fun-9- (minute single char after hour)...ok
        iso8601_tests:461: -parse_error_clauses_test_/0-fun-7- (second single char after colon minute)...ok
        iso8601_tests:462: -parse_error_clauses_test_/0-fun-5- (second_no_colon single char)...ok
        iso8601_tests:463: -parse_error_clauses_test_/0-fun-3- (decimal separator with no digits)...ok
        iso8601_tests:464: -parse_error_clauses_test_/0-fun-1- (offset_minute bad suffix)...ok
        iso8601_tests:474: -apply_duration_sign_test_/0-fun-5- (negative year subtracts)...ok
        iso8601_tests:479: -apply_duration_sign_test_/0-fun-3- (positive/unsigned still adds)...ok
        iso8601_tests:484: -apply_duration_sign_test_/0-fun-1- (compound negative duration)...ok
        iso8601_tests:497: -add_years_leap_day_test_/0-fun-9- (leap day + 1 year clamps to Feb 28)...ok
        iso8601_tests:502: -add_years_leap_day_test_/0-fun-7- (leap day - 1 year clamps to Feb 28)...ok
        iso8601_tests:507: -add_years_leap_day_test_/0-fun-5- (leap day + 3 years produces valid date)...ok
        iso8601_tests:513: -add_years_leap_day_test_/0-fun-3- (ordinary date unchanged)...ok
        iso8601_tests:518: -add_years_leap_day_test_/0-fun-1- (via apply_duration years path)...ok
        iso8601_tests: ordinal_parse_no_stdout_test...ok
        iso8601_tests: capture_output_restores_leader_on_error_test...ok
        iso8601_tests:575: -parse_interval_forms_test_/0-fun-15- (start/end)...ok
        iso8601_tests:580: -parse_interval_forms_test_/0-fun-13- (start/duration)...ok
        iso8601_tests:585: -parse_interval_forms_test_/0-fun-11- (duration/end)...ok
        iso8601_tests:590: -parse_interval_forms_test_/0-fun-9- (duration only)...ok
        iso8601_tests:595: -parse_interval_forms_test_/0-fun-7- (double-hyphen separator)...ok
        iso8601_tests:599: -parse_interval_forms_test_/0-fun-5- (two durations is badarg)...ok
        iso8601_tests:600: -parse_interval_forms_test_/0-fun-3- (three parts is badarg)...ok
        iso8601_tests:602: -parse_interval_forms_test_/0-fun-1- (binary input)...ok
        iso8601_tests:615: -interval_bounds_test_/0-fun-7- (start/end returns both)...ok
        iso8601_tests:624: -interval_bounds_test_/0-fun-5- (start+duration end matches apply_duration)...ok
        iso8601_tests:636: -interval_bounds_test_/0-fun-3- (duration/end start computation)...ok
        iso8601_tests:648: -interval_bounds_test_/0-fun-1- (duration-only is badarg)...ok
        iso8601_tests:658: -interval_signed_duration_test_/0-fun-3- (negative duration in start/duration)...[0.001 s] ok
        iso8601_tests:663: -interval_signed_duration_test_/0-fun-1- (negative duration bounds subtract)...ok
        iso8601_tests:681: -format_interval_test_/0-fun-7- (format start/end)...ok
        iso8601_tests:688: -format_interval_test_/0-fun-5- (format start/duration)...ok
        iso8601_tests:697: -format_interval_test_/0-fun-3- (format duration/end)...ok
        iso8601_tests:706: -format_interval_test_/0-fun-1- (format duration only)...ok
        iso8601_tests:722: -format_interval_roundtrip_test_/0-fun-2- (roundtrip: 2007-03-01T13:00:00Z/2008-05-11T15:30:00Z)...ok
        iso8601_tests:722: -format_interval_roundtrip_test_/0-fun-2- (roundtrip: 2007-03-01T13:00:00Z/P1Y2M10DT2H30M)...ok
        iso8601_tests:722: -format_interval_roundtrip_test_/0-fun-2- (roundtrip: P1Y2M10DT2H30M/2008-05-11T15:30:00Z)...[0.001 s] ok
        iso8601_tests:722: -format_interval_roundtrip_test_/0-fun-2- (roundtrip: P1Y2M10DT2H30M)...ok
        iso8601_tests:743: -interval_inheritance_test_/0-fun-9- (inherit date (time-only end))...ok
        iso8601_tests:753: -interval_inheritance_test_/0-fun-7- (inherit year (month-day end))...ok
        iso8601_tests:763: -interval_inheritance_test_/0-fun-5- (inherit year+month (day-only end))...ok
        iso8601_tests:773: -interval_inheritance_test_/0-fun-3- (inherit year+month (day+time end))...ok
        iso8601_tests:783: -interval_inheritance_test_/0-fun-1- (ambiguous single digit is badarg)...ok
        iso8601_tests:793: -interval_coverage_test_/0-fun-11- (non-duration no-separator string is badarg)...ok
        iso8601_tests:795: -interval_coverage_test_/0-fun-9- (positive-signed duration (+P1Y))...ok
        iso8601_tests:800: -interval_coverage_test_/0-fun-7- (fractional second endpoint uses parse_exact)...ok
        iso8601_tests:805: -interval_coverage_test_/0-fun-5- (date-only duration (no time components))...ok
        iso8601_tests:820: -interval_coverage_test_/0-fun-3- (inherit with T-prefixed end fragment)...ok
        iso8601_tests:830: -interval_coverage_test_/0-fun-1- (format date-only duration (time parts empty))...ok
        iso8601_tests:853: -interval_minute_precision_test_/0-fun-3- (time-only end at minute precision)...ok
        iso8601_tests:863: -interval_minute_precision_test_/0-fun-1- (day+time end at minute precision)...ok
        iso8601_tests:881: -interval_malformed_end_test_/0-fun-3- (double T in end fragment)...ok
        iso8601_tests:883: -interval_malformed_end_test_/0-fun-1- (consecutive TT in end fragment)...ok
        iso8601_tests:892: -interval_empty_sides_test_/0-fun-11- (double slash)...ok
        iso8601_tests:893: -interval_empty_sides_test_/0-fun-9- (leading slash)...ok
        iso8601_tests:894: -interval_empty_sides_test_/0-fun-7- (trailing slash)...ok
        iso8601_tests:896: -interval_empty_sides_test_/0-fun-5- (empty double-hyphen sides (quad dash))...ok
        iso8601_tests:898: -interval_empty_sides_test_/0-fun-3- (leading double-hyphen (empty left))...ok
        iso8601_tests:900: -interval_empty_sides_test_/0-fun-1- (trailing double-hyphen (empty right))...ok
        iso8601_tests:910: -interval_basic_format_start_test_/0-fun-1- (basic-format start with abbreviated end is badarg)...ok
        iso8601_tests:920: -interval_month_day_time_end_test_/0-fun-1- (month-day+time abbreviated end is badarg)...ok
        iso8601_tests:930: -expanded_year_test_/0-fun-17- (explicit + sign with leading zeros)...ok
        iso8601_tests:932: -expanded_year_test_/0-fun-15- (+2007 is year 2007 not 200)...ok
        iso8601_tests:934: -expanded_year_test_/0-fun-13- (year beyond 9999)...ok
        iso8601_tests:935: -expanded_year_test_/0-fun-11- (unsigned overlength is badarg)...ok
        iso8601_tests:936: -expanded_year_test_/0-fun-9- (empty after sign is badarg)...ok
        iso8601_tests:938: -expanded_year_test_/0-fun-7- (+YYYY year only defaults to Jan 1)...ok
        iso8601_tests:940: -expanded_year_test_/0-fun-5- (+YYYY with full datetime)...ok
        iso8601_tests:942: -expanded_year_test_/0-fun-3- (parse_exact with expanded year)...ok
        iso8601_tests:947: -expanded_year_test_/0-fun-1- (format roundtrip with expanded year)...ok
        iso8601_tests:960: -parse_expanded_test_/0-fun-17- (negative year -0001)...ok
        iso8601_tests:962: -parse_expanded_test_/0-fun-15- (negative year -0000 (year zero))...ok
        iso8601_tests:964: -parse_expanded_test_/0-fun-13- (Ides of March 44 BCE (astro -44))...ok
        iso8601_tests:966: -parse_expanded_test_/0-fun-11- (positive expanded +0002007)...ok
        iso8601_tests:968: -parse_expanded_test_/0-fun-9- (year beyond 9999)...ok
        iso8601_tests:970: -parse_expanded_test_/0-fun-7- (plain 4-digit year works too)...ok
        iso8601_tests:971: -parse_expanded_test_/0-fun-5- (empty after sign is badarg)...ok
        iso8601_tests:973: -parse_expanded_test_/0-fun-3- (binary input)...ok
        iso8601_tests:975: -parse_expanded_test_/0-fun-1- (fractional seconds preserved)...ok
        iso8601_tests:983: -parse_rejects_negative_test_/0-fun-3- (parse/1 rejects negative year)...ok
        iso8601_tests:985: -parse_rejects_negative_test_/0-fun-1- (parse_exact/1 rejects negative year)...ok
        iso8601_tests:991: -format_expanded_test_/0-fun-9- (format negative year)...ok
        iso8601_tests:996: -format_expanded_test_/0-fun-7- (format large positive year)...ok
        iso8601_tests:1001: -format_expanded_test_/0-fun-5- (format normal year (explicit sign))...ok
        iso8601_tests:1006: -format_expanded_test_/0-fun-3- (format year zero)...ok
        iso8601_tests:1011: -format_expanded_test_/0-fun-1- (format with float seconds)...ok
        iso8601_tests:1020: -format_expanded_roundtrip_test_/0-fun-3- (roundtrip negative year)...ok
        iso8601_tests:1029: -format_expanded_roundtrip_test_/0-fun-1- (roundtrip large positive year)...ok
        iso8601_tests:1047: -parse_out_of_range_positive_crashes_test_/0-fun-1-...ok
        iso8601_tests:1056: -parse_expanded_negative_offset_test_/0-fun-7- (negative year + timezone offset is badarg)...ok
        iso8601_tests:1058: -parse_expanded_negative_offset_test_/0-fun-5- (negative year + week date crashes)...ok
        iso8601_tests:1060: -parse_expanded_negative_offset_test_/0-fun-3- (negative year in UTC still works)...ok
        iso8601_tests:1065: -parse_expanded_negative_offset_test_/0-fun-1- (negative year plain date still works)...ok
        [done in 0.643 s]
      [done in 0.643 s]
    [done in 0.643 s]
  [done in 0.643 s]
module 'prop_iso8601'
=======================================================
  All 210 tests passed.
```

### rebar3 proper -c

Exit: 0; elapsed 0.73s

```text
[0;32m===> Verifying dependencies...
[0m[0;32m===> Analyzing applications...
[0m[0;32m===> Compiling iso8601
[0m[0;32m===> Testing prop_iso8601:prop_parse_format_roundtrip()
[0m....................................................................................................
[01;32mOK: Passed 100 test(s).
[00m[0;32m===> Testing prop_iso8601:prop_interval_start_end_roundtrip()
[0m....................................................................................................
[01;32mOK: Passed 100 test(s).
[00m[0;32m===> Testing prop_iso8601:prop_start_duration_bounds()
[0m....................................................................................................
[01;32mOK: Passed 100 test(s).
[00m[0;32m===> Testing prop_iso8601:prop_expanded_roundtrip()
[0m....................................................................................................
[01;32mOK: Passed 100 test(s).
[00m[0;32m===>\x20
4/4 properties passed
[0m```

### rebar3 as test cover -v --min_coverage=100

Exit: 0; elapsed 0.69s

```text
[0;32m===> Verifying dependencies...
[0m[0;32m===> Analyzing applications...
[0m[0;32m===> Compiling iso8601
[0m[0;32m===> Performing cover analysis...
[0m  |------------------------|------------|
  |                module  |  coverage  |
  |------------------------|------------|
  |               iso8601  |      100%  |
  |------------------------|------------|
  |                 total  |      100%  |
  |------------------------|------------|
  coverage calculated from:
    /Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit/_build/test/cover/eunit.coverdata
    /Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit/_build/test/cover/proper.coverdata
  cover summary written to: /Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit/_build/test/cover/index.html
```

### rebar3 as dev fmt --check

Exit: 1; elapsed 0.6s

```text
[0;35m===> Failed to update package erlfmt from repo hexpm
[0m[0;35m===> Errors loading plugin erlfmt. Run rebar3 with DEBUG=1 set to see errors.
[0m[0;35m===> Failed to update package rebar3_lint from repo hexpm
[0m[0;35m===> Errors loading plugin rebar3_lint. Run rebar3 with DEBUG=1 set to see errors.
[0m[0;35m===> Failed to update package rebar3_ex_doc from repo hexpm
[0m[0;35m===> Errors loading plugin rebar3_ex_doc. Run rebar3 with DEBUG=1 set to see errors.
[0m[0;35m===> Failed to update package coveralls from repo hexpm
[0m[0;35m===> Errors loading plugin coveralls. Run rebar3 with DEBUG=1 set to see errors.
[0m[0;31m===> [1mCommand fmt not found
[0m[0m```

### rebar3 as dev lint

Exit: 1; elapsed 0.57s

```text
[0;35m===> Failed to update package erlfmt from repo hexpm
[0m[0;35m===> Errors loading plugin erlfmt. Run rebar3 with DEBUG=1 set to see errors.
[0m[0;35m===> Failed to update package rebar3_lint from repo hexpm
[0m[0;35m===> Errors loading plugin rebar3_lint. Run rebar3 with DEBUG=1 set to see errors.
[0m[0;35m===> Failed to update package rebar3_ex_doc from repo hexpm
[0m[0;35m===> Errors loading plugin rebar3_ex_doc. Run rebar3 with DEBUG=1 set to see errors.
[0m[0;35m===> Failed to update package coveralls from repo hexpm
[0m[0;35m===> Errors loading plugin coveralls. Run rebar3 with DEBUG=1 set to see errors.
[0m[0;31m===> [1mCommand lint not found
[0m[0m```

### escript /Users/oubiwann/lab/erlsci/iso8601/.worktrees/planning/project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/date-probes.escript

Exit: 0; elapsed 0.28s

```text
parse_baseline => {return,{{2012,2,16},{1,6,48}}}
parse_exact_baseline => {return,{{2012,2,3},{4,5,6.5}}}
parse_positive_expanded => {return,{{10000,1,1},{0,0,0}}}
parse_expanded_negative_control => {return,{{-1,1,1},{0,0,0.0}}}
parse_rejects_negative_control => {exception,error,badarg}
ordinal_leap_control => {return,{{2016,2,29},{0,0,0}}}
week_boundary_control => {return,{{2008,12,29},{0,0,0}}}
week_last_valid_control => {return,{{2010,1,3},{0,0,0}}}
week_2015_w01_control => {return,{{2014,12,29},{0,0,0}}}
week_zero_invalid => {return,{{2013,12,23},{0,0,0}}}
week_53_invalid_for_2014 => {return,{{2014,12,29},{0,0,0}}}
week_day_8_invalid => {return,{{2014,1,6},{0,0,0}}}
flat_list => {return,{{2012,2,16},{0,0,0}}}
binary => {return,{{2012,2,16},{0,0,0}}}
nested_iodata_parse => {exception,error,badarg}
nested_iodata_exact => {exception,error,badarg}
nested_iodata_expanded => {exception,error,badarg}
format_epoch_control => {return,<<"1970-01-01T00:00:00Z">>}
format_fraction_control => {return,<<"2012-02-03T04:05:06.500000Z">>}
parse_regular_invalid_calendar_control => {exception,error,if_clause}
parse_expanded_invalid_nonleap_feb29 => {return,{{-1,2,29},{0,0,0.0}}}
parse_expanded_year_zero_valid_leap_control => {return,{{0,2,29},{0,0,0.0}}}
parse_expanded_year_zero_invalid_feb30 => {return,{{0,2,30},{0,0,0.0}}}
parse_expanded_invalid_feb30 => {return,{{-1,2,30},{0,0,0.0}}}
parse_expanded_invalid_month => {return,{{-1,13,1},{0,0,0.0}}}
parse_expanded_invalid_day_zero => {return,{{-1,1,0},{0,0,0.0}}}
parse_expanded_valid_negative_leap_control => {return,{{-4,2,29},{0,0,0.0}}}
format_float_below_roundover => {return,<<"2012-12-30T23:59:59.999999Z">>}
format_float_roundover_nonleap_day => {return,
                                       <<"2012-12-30T23:59:60.000000Z">>}
parse_format_roundover_nonleap_day => {return,{{2012,12,31},{0,0,0}}}
format_expanded_float_roundover_nonleap_day => {return,
                                                <<"-0001-12-30T23:59:60.000000Z">>}
```

### git diff --exit-code bcfd94a --

Exit: 0; elapsed 0.02s

```text
```

### CDC independent discriminating fractional-unit checks

Fresh CDC erlc beam from unchanged bcfd94a at `/private/tmp/iso8601-p02-cdc`; command `erl -noshell -pa /private/tmp/iso8601-p02-cdc -eval` with exact expression below; exit0. Expected seconds independently derived from 0.0001×3600=0.36, 0.0002×3600=0.72, 0.001×60=0.06; these are not implementation-vs-itself oracles.

```erlang
Cases=[{"fractional-hour .0001",fun()->iso8601:parse_exact("2012-02-03T04.0001")end,{{2012,2,3},{4,0,0.36}}},{"fractional-hour .0002",fun()->iso8601:parse_exact("2012-02-03T04.0002")end,{{2012,2,3},{4,0,0.72}}},{"fractional-minute .001",fun()->iso8601:parse_exact("2012-02-03T04:05.001")end,{{2012,2,3},{4,5,0.06}}},{"whole-second fractional-hour control",fun()->iso8601:parse_exact("2012-02-03T04.25")end,{{2012,2,3},{4,15,0.0}}},{"fractional-second control",fun()->iso8601:parse_exact("2012-02-03T04:05:00.36")end,{{2012,2,3},{4,5,0.36}}}],lists:foreach(fun({N,F,E})->A=F(),io:format("~s expected=~p actual=~p match=~p~n",[N,E,A,E=:=A])end,Cases), io:format("independent week oracle=~p invalid year0 date=~p microsecond-rounding=~p~n",[calendar:iso_week_number({2014,12,29}),calendar:valid_date({0,2,30}),round(59.9999999*1000000)]),halt().
```

```text
fractional-hour .0001 expected={{2012,2,3},{4,0,0.36}} actual={{2012,2,3},
                                                               {4,0,0.0}} match=false
fractional-hour .0002 expected={{2012,2,3},{4,0,0.72}} actual={{2012,2,3},
                                                               {4,0,1.0}} match=false
fractional-minute .001 expected={{2012,2,3},{4,5,0.06}} actual={{2012,2,3},
                                                                {4,5,0.0}} match=false
whole-second fractional-hour control expected={{2012,2,3},{4,15,0.0}} actual={{2012,
                                                                               2,
                                                                               3},
                                                                              {4,
                                                                               15,
                                                                               0.0}} match=true
fractional-second control expected={{2012,2,3},{4,5,0.36}} actual={{2012,2,3},
                                                                   {4,5,0.36}} match=true
independent week oracle={2015,1} invalid year0 date=false microsecond-rounding=60000000
```

## Refinement01 execution — 2026-10-31

Current assignment: `cc-prompt-iteration01.md`, issued at planning revision `3bebf0ac0d562c7fe4a06e18cf2e3142047cfdf2`; predecessor assignment `cc-prompt.md`; initial candidate `3dc34cf1c815c2df579505e3570590303cfe3e8c`. Current source remains `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit`, `codex/project02-arc01-audit`, `bcfd94a08fa7a427eb1469679897e4d92d808edf`. Planning branch was clean at the assignment revision before this iteration. CDC returned writes to CC for only the Slice01 artifact directory, closing report, and evidence/status fields in `ledger.md`; no source, prompt, slice plan, parent plan, or reviewer-record changes are in scope.

The current prompt, slice plan, ledger, CDC verification, current report, original probe runner/output, closing report, and this run log were checked against the iteration handoff. The original required-reading manifest is the section embedded in `cc-prompt.md`; no separate manifest file exists or is required. The original prompt's full reads and all 17 Erlang guide chapters remain applicable and were recorded above; chapter 11 was loaded first. For this correction, the source export list and changed date-helper path were refreshed at `src/iso8601.erl:3–19`, `121–147`, `299–320`, `429–441`, and `491–505`; README's exact-precision statement at `README.md:51–53` and the fractional-hour/minute tests at `test/iso8601_tests.erl:54–90`, `429–436` were also refreshed. The full guide02 finding shape was reread and applied: every finding now has an explicit **Scale**, and the report has a scale-coverage section.

### Refinement01 source-cited contract readback addendum

The module exports only the functions listed at `src/iso8601.erl:3–19`; `make_date/4` at 456–462 has a private spec and is not exported. Fractional-hour and fractional-minute syntax is dispatched by `hour_decimal/2` and `minute_decimal/2` (`299–320`); `datetime/1` calls `add_decimal/2` before applying any explicit UTC offset (`429–441`), and `add_decimal/2` passes fractional values into `apply_offset/4` (`429–432`). `apply_offset/4` converts the offset to seconds and rounds it to an integer before returning (`491–505`). `parse_exact/1` and `parse_expanded/1` then add only the distinct `second_decimal` value (`129–147`). The README promises microsecond precision for `parse_exact/1` fractional seconds (`51–53`), while existing fractional-hour/minute tests exercise only values that become whole seconds (`test/iso8601_tests.erl:54–90`, `429–436`). This supports a scoped exact-API finding; it does not alter `parse/1`'s integer-second truncation contract or establish an ISO-version claim.

### R1 — public fractional-unit probes and contract disposition

The fresh source-checkout beam used by the original probe run remains tied to unchanged source `bcfd94a`; CDC also retained its independent fresh `erlc` beam/output above. The focused CC runner is `artifacts/fractional-unit-probes-iteration01.escript`; command `escript artifacts/fractional-unit-probes-iteration01.escript > artifacts/fractional-unit-probes-iteration01.out` exited 0. Runner SHA-256 `2ac497c4b06d41488d07f86e490e64021ae5460e6f1a27efeb4c2b3843d5aed4`; output SHA-256 `e888444d0ac2083978c8aa92cf5ee95beed2cf0e0b9a3aa6f8f6e2d24ebfb9f8`. A failed initial escript compile was corrected before the successful run; exact first failure was:

```text
.../fractional-unit-probes-iteration01.escript:32:5: syntax error before: ']'
escript: There were compilation errors.
exit=1
```

The output file preserves the complete ten-case transcript. Independent expectations use fraction×3600 or fraction×60. The runner states absolute tolerance `1e-12`; expected `0.0001×3600` is represented by Erlang as `0.36000000000000004`. Three fractional-unit subsecond cases fail that tolerance; comma hour/minute, basic-binary hour and expanded-year hour siblings reproduce the mismatch. The whole-second fractional-hour control and explicit fractional-second dot/comma controls pass. The mechanism is concrete: `datetime/1` routes fractional hours/minutes through `add_decimal/2` to `apply_offset/4`, which rounds to whole seconds before `parse_exact/1` and `parse_expanded/1` add the distinct `second_decimal` value. Existing EUnit cases at 54–90 and 429–436 establish support for fractional hour/minute forms but cover only integral-second conversions. README's microsecond-preservation claim for `parse_exact/1` supports reporting this as ERL-DATE-005, Medium, with medium-high confidence; because EDoc/readme do not explicitly discuss subsecond fractional-hour/minute inputs, the report states that limit and does not extend the finding to `parse/1`'s integer-second contract. No ISO conformance claim was needed.

Exact CC runner observations (full Erlang terms and tolerance are in the linked output artifact):

```text
hour_dot_0001: expected second 0.36000000000000004; actual 0.0; false
hour_dot_0002: expected second 0.7200000000000001; actual 1.0; false
minute_dot_001: expected second 0.06; actual 0.0; false
hour_comma: expected second 0.36000000000000004; actual 0.0; false
basic_binary_hour_comma: expected second 0.36000000000000004; actual 0.0; false
minute_comma: expected second 0.06; actual 0.0; false
expanded_hour_fraction: expected second 0.36000000000000004; actual 0.0; false
hour_whole_second_control: expected {4,15,0.0}; actual {4,15,0.0}; true
explicit_second_control: expected {4,5,0.36}; actual {4,5,0.36}; true
explicit_second_comma_control: expected {4,5,0.36}; actual {4,5,0.36}; true
absolute comparison tolerance: 1e-12
```

### R2/R3 — visibility and scale correction

The module export list at `src/iso8601.erl:3–19` contains no `make_date/4`; the cited spec at 456–462 is private. The earlier report's “exported helper spec” characterization was inaccurate. The current report calls it a private spec and separately identifies the actual exports. A focused report search found no other claim that a private date helper is exported. The erroneous original wording remains available at pinned candidate `3dc34cf1c815c2df579505e3570590303cfe3e8c`; the current report has a dated refinement-history note rather than rewriting that commit.

ERL-DATE-001–005 now each contain a guide02 `Scale` field. The report explicitly walks line/function, file/module, logical unit, package/library/target, application/service/executable, whole-codebase, and workspace/monorepo scales. This codebase is a single library with no service or multi-member workspace; those absences are recorded rather than fabricated findings.

The prior public transcript `date-probes.out`, original runner `date-probes.escript`, parent-native gate output, CDC rerun observations and initial failure receipts remain unchanged. This refinement reran only the affected fractional-unit probes; no native gate was rerun because the prompt directs that unchanged native validation not be repeated, and CDC had independently reproduced those gates. The original missing-`timeout` receipt, cached network-fallback output, and exact formatter/linter plugin failures remain in their preceding raw log sections. `git diff --exit-code bcfd94a --` and `git status --short --branch` in the source checkout still show no tracked source change.

## CDC review of refinement01 — candidate6054bad, 2026-10-31

Independently executed `escript /Users/oubiwann/lab/erlsci/iso8601/.worktrees/planning/project02-erlang-audit/arc01-runtime-contracts/slice01-date-contracts/artifacts/fractional-unit-probes-iteration01.escript` (exit0), compared complete stdout byte-for-byte with committed iteration01 output: identical. Counted10 named cases:7 arithmetic mismatches,3 passing controls. The script is observational; exit0 alone does not show semantic success. Exact independent arithmetic and expected values appear in the earlier CDC section. Read all refinement diff, runner/output, current ledger/close and source paths; R1–R3 addressed. Existing native/source evidence remains applicable because only audit metadata/probes changed.
